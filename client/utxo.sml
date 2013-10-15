
structure Utxo (* :> UTXO *)  =
   struct

      structure B = Bytestring
      structure BS = Bytesubstring

      (* Make sure LargeInt is big enough. *)
      val () =
         (case LargeInt.precision of
             NONE => ()
           | SOME n =>
                if n < 64 then
                   raise (Fail "LargeInt is too small.")
                else
                   ())


      fun replicate x n l =
         if n = 0 then
            l
         else
            replicate x (n-1) (x :: l)


      fun isAllzero str =
         let
            val sz = B.size str
            
            fun loop i =
               if i >= sz then
                  true
               else
                  B.sub (str, i) = 0w0
                  andalso
                  loop (i+1)
         in
            loop 0
         end


      fun dhash str =
         SHA256.hashBytes (SHA256.hash (Stream.fromTable BS.sub str 0))


      type hash = Bytestring.string
      type pos = Int64.int


      val hashlen = 32

      structure Key =
         struct
            type t = B.string

            fun eq (str1, str2) =
               BS.eq (BS.substring (str1, 0, hashlen), BS.substring (str2, 0, hashlen))

            fun hash str =
               let
                  fun loop i h =
                      if i >= hashlen then
                         h
                      else
                         loop (i+1) (JenkinsHash.hashInc h (ConvertWord.word8ToWord (Bytestring.sub (str, i))))
               in
                  loop 0 0w0
               end
         end

      structure T = DatalessHashTable (structure Key = Key)

      (* Entries in the UTXO table look like:

          byte
          0                31 32      39 40     39+n 
         +-------------------+----------+-----------+
         |      tx hash      |  tx pos  | spend map |
         +-------------------+----------+-----------+

         where m = number of transaction outputs
               n = ceil(m/8)

         Position is little-endian.

         The first transaction output is the least significant bit of the
         first byte of the spend map.  The second output is the least-but-one
         significant bit of the first byte, etc.  The bit is 1 if the output
         is unspent.
      *)

      (* A TrailValid trail contains each of the utxo entries that were gc'ed.  The rest of
         the work to undo can be determined from the block being undone.

         A TrailGeneral trail contains a script to undo the entire block.  This is general
         (and simple) but ties up a lot of space.
      *)

      datatype trail_elem =
         TrailRemove of B.string
       | TrailInsert of B.string

      datatype trail =
         TrailValid of B.string list
       | TrailGeneral of trail_elem list

      val theTable = T.table Constants.utxoTableSize
      val undoCount = ref 0
      val undoQueue : trail IDeque.ideque = IDeque.ideque ()
      
      
      fun reset () =
         (
         T.reset theTable Constants.utxoTableSize;
         undoCount := 0;
         IDeque.reset undoQueue
         )


      fun pushQueue entry =
         (
         IDeque.insertFront undoQueue entry;
         if !undoCount < Constants.maxUndoRecords then
            undoCount := !undoCount + 1
         else
            (
            IDeque.removeBack undoQueue;
            ()
            )
         )


      exception Undo

      fun popQueue () =
         if !undoCount <= 0 then
            raise Undo
         else
            (
            undoCount := !undoCount - 1;
            IDeque.removeFront undoQueue
            )
            

      fun freshEntry hash pos outputCount =
         let
            val posstr =
               ConvertWord.word64ToBytesL (Word64.fromLargeInt (Int64.toLarge pos))

            val spendMapByteTail =
               if outputCount mod 8 = 0 then
                  []
               else
                  [B.str (Word8.>> (0wxff, Word.fromInt (8 - outputCount mod 8)))]

            val spendmap =
               replicate (B.str 0wxff) (outputCount div 8) spendMapByteTail
         in
            B.concat (hash :: posstr :: spendmap)
         end
         

      fun updateEntry entry n =
         let
            val i = n div 8
            val j = n mod 8
            val i' = i + 40

            (* clear bit j of byte i *)

            val b = B.sub (entry, i')
            val mask = Word8.<< (0w1, Word.fromInt j)
            val b' = Word8.andb (b, Word8.notb mask)

            val spendmap =
               BS.concat
               [ BS.substring (entry, 40, i),
                 BS.full (B.str b'),
                 BS.extract (entry, i'+1, NONE) ]
         in
            (BS.concat [BS.substring (entry, 0, 40), BS.full spendmap],
             isAllzero spendmap)
         end

      
      fun traverseBlock f accInitial blockstr =
         let
            val (_, blockstr') = Reader.readS Block.headerReader (BS.full blockstr)
            val (count, txsstr) = Reader.readS Reader.varint blockstr'

            val pos = B.size blockstr - BS.size txsstr

            fun loop i pos str acc =
               if i >= count then
                  acc
               else
                  let
                     val (tx, str') = Reader.readS Transaction.reader str
                     val txsz = BS.size str - BS.size str'
                     val txstr = BS.slice (str, 0, SOME txsz)

                     val acc' = f (i, pos, tx, txstr, acc)
                  in
                     loop (i+1) (pos + txsz) str' acc'
                  end
         in
            loop 0 0 txsstr accInitial
         end


      fun processTxValid isCoinbase pos ({inputs, outputs, ...}:Transaction.tx) hash trail =
         let
            val trail =
               if isCoinbase then
                  (* Don't process the inputs for coinbase transactions. *)
                  trail
               else
                  foldl
                  (fn ({ from=(inhash, n), ... }, trail) =>
                      (case T.find theTable inhash of
                          NONE => raise (Fail "invalid txout")
                        | SOME entry =>
                             let
                                val (entry', allspent) = updateEntry entry n
                             in
                                if allspent then
                                   (
                                   T.remove theTable inhash;
                                   entry' :: trail
                                   )
                                else
                                   (
                                   T.insert theTable entry';
                                   trail
                                   )
                             end))
                  trail
                  inputs

            val () = T.insert theTable (freshEntry hash pos (length outputs))
         in
            trail
         end


      fun processTxGeneral isCoinbase pos ({inputs, outputs, ...}:Transaction.tx) hash trail =
         let
            val trail =
               if isCoinbase then
                  (* Don't process the inputs for coinbase transactions. *)
                  trail
               else
                  foldl
                  (fn ({ from=(inhash, n), ... }, trail) =>
                      (case T.find theTable inhash of
                          NONE =>
                             (* This is invalid, but we allow it. *)
                             trail
                        | SOME entry =>
                             let
                                val (entry', allspent) = updateEntry entry n
                             in
                                if allspent then
                                   T.remove theTable inhash
                                else
                                   T.insert theTable entry';

                                TrailInsert entry :: trail
                             end))
                  trail
                  inputs

            val newentry = freshEntry hash pos (length outputs)
            val oldentryo = T.swap theTable newentry

            val trail =
               (case oldentryo of
                   NONE =>
                      TrailRemove newentry
                 | SOME oldentry =>
                      TrailInsert oldentry) :: trail
         in
            trail
         end


      fun process isValid blockpos blockstr =
         if isValid then
            let
               val trail =
                  traverseBlock
                     (fn (i, pos, tx, txstr, trail) =>
                         processTxValid (i=0) (blockpos + Int64.fromInt pos) tx (dhash txstr) trail)
                     []
                     blockstr
            in
               pushQueue (TrailValid trail)
            end
         else
            let
               val trail =
                  traverseBlock
                     (fn (i, pos, tx, txstr, trail) =>
                         processTxGeneral (i=0) (blockpos + Int64.fromInt pos) tx (dhash txstr) trail)
                     []
                     blockstr
            in
               pushQueue (TrailGeneral trail)
            end


      fun undoTxValid isCoinbase ({inputs, ...}:Transaction.tx) hash =
         let
            val () =
               (* Don't process the inputs for coinbase transactions. *)
               if isCoinbase then
                  ()
               else
                  app
                  (fn { from=(inhash, n), ... } =>
                      (case T.find theTable inhash of
                          NONE =>
                             (* We should have put all the gc'ed transactions back in the utxo before this. *)
                             raise (Fail "transaction absent")
                        | SOME entry =>
                             let
                                val i = n div 8
                                val j = n mod 8
                                val i' = i + 40

                                (* bit j of byte i must be 0 (although we don't check this), set it *)
                                val b = B.sub (entry, i')
                                val b' = Word8.orb (b, Word8.<< (0w1, Word.fromInt j))

                                val entry' =
                                   BS.concat [BS.substring (entry, 0, i'),
                                              BS.full (B.str b'),
                                              BS.extract (entry, i'+1, NONE) ]
                             in
                                T.insert theTable entry'
                             end))
                  inputs

            val () = T.remove theTable hash
         in
            ()
         end


      fun undo blockstr =
         (case popQueue () of
             TrailValid trail =>
                let
                   (* Reinsert gc'ed entries. *)
                   val () =
                      app (fn entry => T.insert theTable entry) trail
                      
                   (* Need to undo the transactions in reverse order (since a later tx might
                      depend on an earlier one), so we assemble a list rather that doing it inline.
                   *)
                   val undos =
                      traverseBlock
                         (fn (i, pos, tx, txstr, l) => (i=0, tx, dhash txstr) :: l)
                         []
                         blockstr
                in
                   app (fn (isCoinbase, tx, hash) => undoTxValid isCoinbase tx hash) undos
                end
           | TrailGeneral trail =>
                app
                   (fn (TrailRemove entry) => T.remove theTable entry
                     | (TrailInsert entry) => T.insert theTable entry)
                   trail)

      
      exception TxoInvalid

      fun lookup (hash, n) =
         (case T.find theTable hash of
             NONE => raise TxoInvalid
           | SOME entry =>
                let
                   val i = n div 8 + 40
                   val j = n mod 8

                   val b = B.sub (entry, i)
                   val mask = Word8.<< (0w1, Word.fromInt j)

                   val () =
                      if Word8.andb (b, mask) = 0w0 then
                         raise TxoInvalid
                      else
                         ()
                in
                   Int64.fromLarge
                   (Word64.toLargeInt
                       (ConvertWord.bytesToWord64SL (BS.substring (entry, 32, 8))))
                end)


      (* I/O *)

      structure W = Writer
      structure R = Reader

      fun >>= (r, f) = R.bind r f
      fun >> (r, r') = R.seq r r'
      fun >>> (w, w') = W.seq w w'
      infixr 3 >>= >> >>>

      fun fileExists filename =
         (OS.FileSys.fileSize filename; true)
         handle OS.SysErr _ => false
              | Overflow => true

      fun must esc f (s, n) =
         let
            val str = f (s, n)
         in
            if B.size str = n then
               str
            else
               esc ()
         end

      fun decodeInstream esc ins d =
         (case d of
             Decoder.ANSWER x => x
           | Decoder.INPUT f =>
                (case BinIO.input1 ins of
                    NONE =>
                       esc ()
                  | SOME b =>
                       decodeInstream esc ins (f b)))


      (* Should find a way to do this better. *)
      fun int64ToBytesL x = 
         ConvertWord.word64ToBytesL (ConvertWord.intInfToWord64 (Int64.toLarge x))

      fun bytesToInt64L str =
         Int64.fromLarge (ConvertWord.word64ToIntInf (ConvertWord.bytesToWord64L str))


      (* The UTXO file has the form:
         8 bytes: final position, little-endian
         4 bytes: number of UTXO table entries, little-endian (m)
         ? bytes: m UTXO table entries

         A UTXO table entry looks like:
         ? byte: varint specifying the size of the entry (n, which is at least 41)
         n bytes: the entry

         This assumes that no entry is ever larger than 255 bytes.  That would be 1720 outputs.
      *)
      
      fun writeTable finalpos =
         let
            val () = Log.long (fn () => "Writing UTXO table")

            val path = OS.Path.concat (Constants.dataDirectory, Chain.utxoFile ^ ".new")
            val path' = OS.Path.concat (Constants.dataDirectory, Chain.utxoFile)
            val outs = BinIO.openOut path
         in
            BinIO.output (outs, int64ToBytesL finalpos);
            BinIO.output (outs, ConvertWord.word32ToBytesL (Word32.fromInt (T.size theTable)));
            T.app (fn entry => W.writeOutstream outs (W.bytesVar entry)) theTable;
            BinIO.closeOut outs;
            OS.FileSys.rename {old=path, new=path'};
            Log.long (fn () => "UTXO table written")
         end
         handle OS.SysErr _ => Log.long (fn () => "Error writing index")


      exception ReadTable
      fun readTable finalpos =
         let
            val path = OS.Path.concat (Constants.dataDirectory, Chain.utxoFile)
            fun esc () = raise ReadTable
         in
            if fileExists path then
               let
                  val () = Log.long (fn () => "Loading UTXO table")
                  val ins = BinIO.openIn path
               in
                  let
                     val finalpos' = bytesToInt64L (must esc BinIO.inputN (ins, 8))
                     val () =
                        if finalpos = finalpos' then
                           ()
                        else
                           (
                           Log.long (fn () => "UTXO table is inconsistent with index");
                           raise ReadTable
                           )

                     val entries = Word32.toInt (ConvertWord.bytesToWord32L (must esc BinIO.inputN (ins, 4)))

                     fun loop i =
                        if i >= entries then
                           ()
                        else
                           let
                              val sz = decodeInstream esc ins Decoder.varint

                              val () =
                                 if sz < 41 then
                                    raise ReadTable
                                 else
                                    ()

                              val str = must esc BinIO.inputN (ins, sz)
                           in
                              T.insert theTable str;
                              loop (i+1)
                           end
                  in
                     loop 0;
                     Log.long (fn () => "Finished loading UTXO table");
                     true
                  end
                  handle ReadTable =>
                     let in
                        BinIO.closeIn ins;
                        Log.long (fn () => "Failed to load UTXO table");
                        T.reset theTable Constants.utxoTableSize;
                        false
                      end
               end
            else
               false
         end

   end
