
structure Blockchain :> BLOCKCHAIN =
   struct

      (* Constants *)
      val tableSize = 0x100000
      val primaryForkSize = 0x080000
      val orphanTableSize = 61
      val maxOrphans = 30         (* Only keep this many orphans around. *)


      structure A = Array
      structure B = Bytestring
      structure BS = Bytesubstring


      (* Precomputed data *)
      val genesisRecord =
         B.concat [Chain.genesisHash, ConvertWord.word32ToBytesL (Word32.fromInt (B.size Chain.genesisBlock)), Chain.genesisBlock]



      (* I/O *)

      type pos = RAIO.pos

      exception BlockchainIO

      (* create dummy streams so we don't need to use options *)
      val dummyOutstream = BinIO.mkOutstream (BinIO.StreamIO.mkOutstream (RAIO.toWriter RAIO.dummyOut, IO.NO_BUF))

      val theOutstream = ref dummyOutstream
      val theRainstream = ref RAIO.dummyIn

      fun getOutPos outs = BinIO.StreamIO.filePosOut (BinIO.StreamIO.getPosOut (BinIO.getOutstream outs))
      fun getInPos ins = BinIO.StreamIO.filePosIn (BinIO.getInstream ins)

      fun rainput pos n =
         let
            val () = RAIO.seekIn (!theRainstream, pos)
            val str = RAIO.inputN (!theRainstream, n)
         in
            if B.size str <> n then
               raise BlockchainIO
            else
               str
         end

      fun outputBlock hash blstr =
         let
            val pos = getOutPos (!theOutstream)
            val outs = !theOutstream
         in
            BinIO.output (outs, hash);
            BinIO.output (outs, ConvertWord.word32ToBytesL (Word32.fromInt (B.size blstr)));
            BinIO.output (outs, blstr);
            BinIO.flushOut outs;
            pos
         end

      fun inputHash pos =
         let
            val rainstream = !theRainstream
         in
            RAIO.seekIn (rainstream, pos);
            RAIO.inputN (rainstream, 32)
         end

      fun inputData pos =
         let
            val rainstream = !theRainstream
            val () = RAIO.seekIn (rainstream, pos+32)
            val sz = Word32.toInt (ConvertWord.bytesToWord32L (RAIO.inputN (rainstream, 4)))
         in
            RAIO.inputN (rainstream, sz)
         end

      fun fileExists filename =
         (OS.FileSys.fileSize filename; true)
         handle OS.SysErr _ => false




      type hash = Bytestring.string

      structure HashHashable =
         struct
            type t = hash
            val eq = B.eq

            fun hash str =
               (* The front bytes are pretty much random, so just use those. *)
               ConvertWord.wordLgToWord (PackWord32Big.subVec (str, 0))
         end
      structure T = HashTable (structure Key = HashHashable)


      datatype lineage =
         (* block is on the primary fork: block number *)
         Nil of int

         (* block is on a secondary fork:
            position, block number, predecessor, predecessor's lineage
         *)
       | Cons of pos * int * hash * lineage


      val lastblock = ref 0
      val lasthash = ref Chain.genesisHash
      val theTable : lineage T.table ref = ref (T.table 1)
      val thePrimaryFork : pos A.array = A.array (primaryForkSize, 0)

      val theOrphanTable : B.string T.table = T.table orphanTableSize
      val theOrphanPredTable : hash T.table = T.table orphanTableSize


      (* Takes a hash and lineage of a block to put on the primary fork, displacing
         the fork that is currently there.
         Returns the hash and lineage of the block (of the same number) that used to
         be on the primary fork.
      *)
      fun makePrimary hash lineage =
         (case lineage of
             Nil _ =>
                (hash, lineage)
           | Cons (pos, num, pred, predlin) =>
                let
                   val oldpos = A.sub (thePrimaryFork, num)
                   val oldhash = inputHash oldpos
                   val (oldpred, oldpredlin) = makePrimary pred predlin
                   val oldlin = Cons (oldpos, num, oldpred, oldpredlin)
                in
                   Array.update (thePrimaryFork, num, pos);
                   T.insert (!theTable) hash (Nil num);
                   T.insert (!theTable) oldhash oldlin;
                   (oldhash, oldlin)
                end)


      fun insertBlockMain hash blstr poso =
         if T.member (!theTable) hash then
            false
         else if T.member theOrphanTable hash then
            true
         else
            let
               val prev = B.substring (blstr, 4, 32)
            in
               (case T.find (!theTable) prev of
                   NONE =>
                      (* Previous block is not in the table. This is an orphan block. *)
                      (
                      if T.size theOrphanTable >= maxOrphans then
                         (* Don't want to be burdened with tons of orphans, so purge the table. *)
                         (
                         T.reset theOrphanTable orphanTableSize;
                         T.reset theOrphanPredTable orphanTableSize
                         )
                      else
                         ();

                      T.insert theOrphanTable hash blstr;
                      T.insert theOrphanPredTable prev hash;
                      true
                      )
                 | SOME lineage =>
                      let
                         val pos =
                            (* If we're given a position, use it.  Otherwise write to file. *)
                            (case poso of
                                NONE =>
                                   outputBlock hash blstr
                              | SOME pos =>
                                   pos)

                         val num =
                            (case lineage of
                                Nil n => n
                              | Cons (_, n, _, _) => n) + 1
                      in
                         if num > !lastblock then
                            (* Extending the longest chain. *)
                            (
                            if num mod 100 = 0 then
                               Log.log (fn () => "Block " ^ Int.toString num ^ "\n")
                            else
                               ();

                            makePrimary prev lineage;
                            Array.update (thePrimaryFork, num, pos);
                            T.insert (!theTable) hash (Nil num);
                            lastblock := num;
                            lasthash := hash
                            )
                         else
                            (* On a secondary fork. *)
                            (
                            T.insert (!theTable) hash (Cons (pos, num, prev, lineage));
                            
                            (case lineage of
                                Nil _ => Log.log (fn () => "Fork detected at " ^ B.toStringHex (B.rev hash) ^ "\n")
                              | _ => ())
                            );

                         false
                      end)
            end


      fun insertBlock hash blstr =
         if insertBlockMain hash blstr NONE then
            true
         else
            (* Check whether hash is the predecessor to an orphan.  It so, insert it too. *)
            (case T.find theOrphanPredTable hash of
                SOME hash' =>
                   let
                      val blstr' = T.lookup theOrphanTable hash'
                   in
                      T.remove theOrphanPredTable hash;
                      T.remove theOrphanTable hash';

                      insertBlockMain hash' blstr' NONE
                      (* hash' cannot be be an orphan, because we've just inserted its predecessor
                         (ie, hash itself), therefore this call must return false.
                      *)
                   end
              | NONE =>
                   false)


      fun loadBlock hash blstr pos =
         (
         insertBlockMain hash blstr (SOME pos);
         ()
         )

      fun loadFile instream =
         let
            val pos = getInPos instream
            val hash = BinIO.inputN (instream, 32)
         in
            if B.size hash = 0 then
               BinIO.closeIn instream
            else if B.size hash <> 32 then
               raise BlockchainIO
            else
               let
                  val szstr = BinIO.inputN (instream, 4)
                  val sz =
                     Word32.toInt (ConvertWord.bytesToWord32L szstr)
                     handle ConvertWord => raise BlockchainIO

                  val blstr = BinIO.inputN (instream, sz)
               in
                  if B.size blstr = sz then
                     (
                     loadBlock hash blstr pos;
                     loadFile instream
                     )
                  else
                     raise BlockchainIO
               end
         end

      fun initialize () =
         (
         lastblock := 0;
         lasthash := Chain.genesisHash;
         theTable := T.table tableSize;
         T.reset theOrphanTable orphanTableSize;
         T.reset theOrphanPredTable orphanTableSize;

         if fileExists "blockchain" then
            let
               val instream = BinIO.openIn "blockchain"
               val gensz = B.size Chain.genesisBlock
            in
               theOutstream := BinIO.openAppend "blockchain";
               theRainstream := RAIO.fromInstream (BinIO.openIn "blockchain");

               (* Verify that the first record looks right. *)
               if B.eq (BinIO.inputN (instream, B.size genesisRecord), genesisRecord) then
                  ()
               else
                  raise BlockchainIO;

               T.insert (!theTable) Chain.genesisHash (Nil 0);
               A.update (thePrimaryFork, 0, 0);

               Log.log (fn () => "Loading blockchain file\n");
               loadFile instream;
               Log.log (fn () => "Load complete\n")
            end
         else
            (* start a new blockchain record *)
            let
               val outstream = BinIO.openOut "blockchain"
            in
               theOutstream := outstream;
               theRainstream := RAIO.fromInstream (BinIO.openIn "blockchain");
   
               BinIO.output (outstream, genesisRecord);
               BinIO.flushOut outstream;
               Log.log (fn () => "Created new blockchain file\n");
               T.insert (!theTable) Chain.genesisHash (Nil 0);
               A.update (thePrimaryFork, 0, 0)
            end
         )

      fun close () =
         (
         BinIO.closeOut (!theOutstream);
         theOutstream := dummyOutstream;
         RAIO.closeIn (!theRainstream);
         theRainstream := RAIO.dummyIn
         )



      exception Absent = T.Absent

      fun member hash = T.member (!theTable) hash

      fun blockPosition hash =
         (case T.lookup (!theTable) hash of
             Nil num =>
                A.sub (thePrimaryFork, num)
           | Cons (pos, _, _, _) => pos)

      fun blockNumber hash =
         (case T.lookup (!theTable) hash of
             Nil num => num
           | Cons (_, num, _, _) => num)

      fun blockData hash = inputData (blockPosition hash)

      fun lastBlock () = !lastblock

      fun lastHash () = !lasthash

      fun hashByNumber num =
         if num > !lastblock then
            raise Absent
         else
            inputHash (A.sub (thePrimaryFork, num))

      fun dataByNumber num =
         if num > !lastblock then
            raise Absent
         else
            inputData (A.sub (thePrimaryFork, num))

      fun knownOrphan hash = T.member theOrphanTable hash

   end
