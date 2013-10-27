
structure Utxo :> UTXO =
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

      val maxint = valOf Int.maxInt


      fun repeat n (f : unit -> unit) =
         if n <= 0 then
            ()
         else
            (f (); repeat (n-1) f)


      fun repeatlist n f =
         let
            fun loop n acc =
               if n <= 0 then
                  acc
               else
                  loop (n-1) (f () :: acc)
         in
            loop n []
         end


      fun replicate x n l =
         if n = 0 then
            l
         else
            replicate x (n-1) (x :: l)


      fun isAllzero str i j =
         if i >= j then
            true
         else
            B.sub (str, i) = 0w0
            andalso
            isAllzero str (i+1) j


      fun idequeExtract q =
         let
            fun loop acc =
               if IDeque.isEmpty q then
                  acc
               else
                  let
                     val x = IDeque.removeBack q
                  in
                     loop (x :: acc)
                  end

            val l = loop []
         in
            (* put elements back in *)
            List.app (IDeque.insertBack q) l;
            l
         end



      (* Conversions *)

      (* Should find a way to do 64-bit conversions this better. *)

      fun int64ToBytesL x = 
         ConvertIntInf.toFixedBytesL (8, Int64.toLarge x)

      fun bytesToInt64L str =
         Int64.fromLarge (ConvertIntInf.fromBytesL str)

      fun bytesToWord16L str =
         Word.orb (Word.<< (ConvertWord.word8ToWord (B.sub (str, 1)), 0w8),
                   ConvertWord.word8ToWord (B.sub (str, 0)))



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

      structure T =
         DatalessBranchingTable
         (structure Base = DatalessHashTable (structure Key = Key)
          structure Nursery = HashTable (structure Key = Key)
          val history = Constants.maxUtxoHistory
          val nurseryInit = 2000)

      (* Entries in the UTXO table look like:

          byte
          0                31 32      39 40     39+n 
         +-------------------+----------+-----------+
         |      tx hash      |  tx pos  | spend map |
         +-------------------+----------+-----------+

         or, for coinbase entries:

          byte
          0                31 32      39 40     39+n 40+n 43+n
         +-------------------+----------+-----------+---------+
         |      tx hash      |  tx pos  | spend map | block # |
         +-------------------+----------+-----------+---------+

         where m = number of transaction outputs
               n = ceil(m/8)

         Position and block number are little-endian.

         The least significant bit of the first byte of the spend map indicates
         if it is a coinbase transaction (1 if so, 0 if not).

         The first transaction output is the least-but-one significant bit of the
         first byte of the spend map.  The second output is the least-but-two
         significant bit of the first byte, etc.  The bit is 1 if the output
         is unspent.
      *)

      type table = T.table
      exception Expired = T.Expired
      
      
      fun new () = T.table Constants.utxoTableSize


      val null = T.null


      val branch = T.branch
      

      fun insert table hash pos outputCount =
         let
            val bits = outputCount+1

            val posstr =
               ConvertWord.word64ToBytesL (Word64.fromLargeInt (Int64.toLarge pos))

            val spendmap =
               if bits div 8 = 0 then
                  [B.str (Word8.andb (Word8.>> (0wxff, Word.fromInt (8 - bits mod 8)), 0wxfe))]
               else
                  B.str 0wxfe
                  ::
                  replicate
                     (B.str 0wxff) 
                     (bits div 8 - 1)
                     (if bits mod 8 = 0 then
                         []
                      else
                         [B.str (Word8.>> (0wxff, Word.fromInt (8 - bits mod 8)))])
         in
            T.insert table (B.concat (hash :: posstr :: spendmap))
         end


      fun insertCoinbase table hash pos outputCount blockNumber =
         let
            val bits = outputCount+1

            val posstr =
               ConvertWord.word64ToBytesL (Word64.fromLargeInt (Int64.toLarge pos))

            val blockstr = ConvertWord.word32ToBytesL (Word32.fromInt blockNumber)

            val spendmap =
               replicate 
                  (B.str 0wxff)
                  (bits div 8)
                  (if bits mod 8 = 0 then
                      []
                   else
                      [B.str (Word8.>> (0wxff, Word.fromInt (8 - bits mod 8)))])
         in
            T.insert table (B.concat (hash :: posstr :: spendmap @ [blockstr]))
         end


      fun unspent table blockNumber (hash, n) =
         ((case T.find table hash of
              NONE =>
                 false
            | SOME entry =>
                 let
                    val i = (n+1) div 8 + 40
                    val j = (n+1) mod 8
        
                    val regular = Word8.andb (B.sub (entry, 40), 0w1) = 0w0  (* regular = not coinbase *)

                    (* If regular, this will fail when n is too large.
                       If coinbase, we need to bound-check explicitly, which we do below.
                    *)
                    val b = B.sub (entry, i)

                    val mask = Word8.<< (0w1, Word.fromInt j)
                 in
                    if regular then
                       Word8.andb (b, mask) <> 0w0
                    else
                       let
                          val sz = B.size entry
                       in
                          i < sz-4
                          andalso
                          Word8.andb (b, mask) <> 0w0
                          andalso
                          blockNumber > Word32.toInt (ConvertWord.bytesToWord32L (B.extract (entry, sz-4, NONE))) + Constants.coinbaseMaturity
                       end
                 end)
          handle Subscript => false)


      fun spend table blockNumber (hash, n) =
         ((case T.find table hash of
              NONE =>
                 false
            | SOME entry =>
                 let
                    val i = (n+1) div 8 + 40
                    val j = (n+1) mod 8

                    val regular = Word8.andb (B.sub (entry, 40), 0w1) = 0w0  (* regular = not coinbase *)
                    
                    val sz = B.size entry

                    (* If regular, this will fail when n is too large.
                       If coinbase, we need to bound-check explicitly, which we do below.
                    *)
                    val b = B.sub (entry, i)

                    val mask = Word8.<< (0w1, Word.fromInt j)
                    val b' = Word8.andb (b, Word8.notb mask)
        
                    val entry' =
                       BS.concat [BS.substring (entry, 0, i),
                                  BS.full (B.str b'),
                                  BS.extract (entry, i+1, NONE)]
                 in
                    if regular then
                       let in
                          (* GC the entry if all inputs are spent. *)
                          if isAllzero entry' 40 sz then
                             T.remove table hash
                          else
                             T.insert table entry' ;
                       
                          Word8.andb (b, mask) <> 0w0
                       end
                    else
                       let in
                          if i < sz-4 then
                             ()
                          else
                             raise Subscript ;

                          (* GC the entry if all inputs are spent. *)
                          if
                             (* Mask out first bit, and skip the last 4 bytes. *)
                             Word8.andb (B.sub (entry', 40), 0wxfe) = 0w0
                             andalso
                             isAllzero entry' 41 (sz-4)
                          then
                             T.remove table hash
                          else
                             T.insert table entry' ;

                          Word8.andb (b, mask) <> 0w0
                          andalso
                          blockNumber >= Word32.toInt (ConvertWord.bytesToWord32L (B.extract (entry, sz-4, NONE))) + Constants.coinbaseMaturity
                       end
                 end)
          handle Subscript => false)


      fun processBlock table blockpos blocknumber blockstr =
         Block.traverseBlock
            (fn (i, pos, {inputs, outputs, ...}, txstr, ()) =>
                if i = 0 then
                   (* Don't process the inputs for coinbase transactions. *)
                   insertCoinbase table (dhash txstr) (blockpos + Int64.fromInt pos) (length outputs) blocknumber
                else
                   let in
                      app (fn {from, ...} => (spend table maxint from; ())) inputs;
                      insert table (dhash txstr) (blockpos + Int64.fromInt pos) (length outputs)
                   end)
            ()
            blockstr


      fun find table hash =
         (case T.find table hash of
             NONE => NONE
           | SOME entry =>
                SOME
                (Int64.fromLarge
                    (Word64.toLargeInt
                       (ConvertWord.bytesToWord64SL (BS.substring (entry, 32, 8))))))



      (* I/O *)

      fun fileExists filename =
         (OS.FileSys.fileSize filename; true)
         handle OS.SysErr _ => false
              | Overflow => true

      exception ReadTable

      fun inputN ins n =
         let
            val str = BinIO.inputN (ins, n)
         in
            if B.size str = n then
               str
            else
               raise ReadTable
         end

      fun input1 ins =
         (case BinIO.input1 ins of
             NONE =>
                raise ReadTable
           | SOME b =>
                b)

      fun inputWord16L ins = bytesToWord16L (inputN ins 2)

      fun inputWord32L ins = ConvertWord.bytesToWord32L (inputN ins 4)

      fun inputInt32L ins =
         Word32.toInt (inputWord32L ins)
         handle Overflow => raise ReadTable

      structure Varint =
         VarintFun
         (struct
             type 'a m = BinIO.instream -> 'a
             exception SyntaxError = ReadTable
             fun return x ins = x
             fun seq f g ins = (f ins; g ins)
             fun bind f g ins = g (f ins) ins
             val byte = input1
             val word16L = inputWord16L
             val word32L = inputWord32L
          end)

      val inputVarint = Varint.varint

      fun inputEntry ins =
         let
            val sz = inputVarint ins

            val () =
               if sz < 41 then
                  raise ReadTable
               else
                  ()
         in
            inputN ins sz
         end

      fun outputInt32L (outs, x) =
         BinIO.output (outs, ConvertWord.word32ToBytesL (Word32.fromInt x))

      fun outputBytesVar (outs, str) = Writer.writeOutstream outs (Writer.bytesVar str)
      

      (* The UTXO file is a sequence of UTXO records.  Each record consists of
         a 1-byte tag, followed by the record data.  Branches are written starting
         with the oldest.

         Branch terminator (tag=1)
         no data

         Table terminator (tag=2)
         no data
         replaces the final branch terminator

         Insertion record (tag=3)
         ? bytes: varint specifying the size of the entry (n, which is at least 41)
         n bytes: the entry

         Deletion record (tag=4)
         32 bytes: the key to delete

      *)


      fun writeTables outs table =
         let
            val () = Log.long (fn () => "Writing UTXO data")

            fun loop t =
               let in
                  (case T.parent t of
                      NONE =>
                         ()
                    | SOME t' =>
                         (
                         loop t';
                         BinIO.output1 (outs, 0w1)
                         ));

                  T.foldDiff
                     (fn (key, ()) =>
                         let in
                            BinIO.output1 (outs, 0w3);
                            outputBytesVar (outs, key)
                         end)
                     (fn (key, ()) =>
                         let in
                            BinIO.output1 (outs, 0w4);
                            BinIO.output (outs, B.substring (key, 0, 32))
                         end)
                     ()
                     t
               end
         in
            loop table;
            BinIO.output1 (outs, 0w2);
            Log.long (fn () => "UTXO data written")
         end
         handle OS.SysErr _ => Log.long (fn () => "Error writing UTXO data")


      fun readTables ins =
         let
            val () = Log.long (fn () => "Reading UTXO data")

            fun loop cur acc =
               (case input1 ins of
                   0w1 =>
                      (* end of branch *)
                      loop (T.branch cur) (cur :: acc)

                 | 0w2 =>
                      (* end of table *)
                      cur :: acc

                 | 0w3 =>
                      (* insertion record *)
                      (
                      T.insert cur (inputEntry ins);
                      loop cur acc
                      )

                 | 0w4 =>
                      (* deletion record *)
                      (
                      T.remove cur (inputN ins 32);
                      loop cur acc
                      )

                 | _ =>
                      raise ReadTable)

            val tables =
               loop (T.table Constants.utxoTableSize) []
         in
            Log.long (fn () => "Finished reading UTXO data");
            SOME tables
         end
         handle ReadTable =>
            let in
               Log.long (fn () => "Failed to read UTXO data");
               NONE
             end

   end
