
structure Blockchain :> BLOCKCHAIN =
   struct

      (* Constants *)
      val tableSize = 0x100000
      val arraySize = 0x080000


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
      val theTable : lineage T.table ref = ref (T.table 1)
      val thePrimaryFork : pos A.array = A.array (arraySize, 0)



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


      fun insertBlockMain hash blstr posthunk =
         if T.member (!theTable) hash then
            ()
         else
            let
               val prev = B.substring (blstr, 4, 32)
            in
               (case T.find (!theTable) prev of
                   NONE =>
                      (* Previous block is not in the table. This is an orphan block, ignore. *)
                      ()
                 | SOME lineage =>
                      let
                         val pos = posthunk ()

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
                            lastblock := num
                            )
                         else
                            (* On a secondary fork. *)
                            (
                            T.insert (!theTable) hash (Cons (pos, num, prev, lineage));
                            
                            (case lineage of
                                Nil _ => Log.log (fn () => "Fork detected at " ^ B.toStringHex (B.rev hash) ^ "\n")
                              | _ => ())
                            )
                      end)
            end

      fun insertBlock hash blstr =
         insertBlockMain hash blstr (fn () => outputBlock hash blstr)

      fun loadBlock hash blstr pos =
         insertBlockMain hash blstr (fn () => pos)

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
         if fileExists "blockchain" then
            let
               val instream = BinIO.openIn "blockchain"
               val gensz = B.size Chain.genesisBlock
            in
               theOutstream := BinIO.openAppend "blockchain";
               theRainstream := RAIO.fromInstream (BinIO.openIn "blockchain");
               lastblock := 0;
               theTable := T.table tableSize;

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
               lastblock := 0;
               theTable := T.table tableSize;
   
               BinIO.output (outstream, genesisRecord);
               BinIO.flushOut outstream;
               Log.log (fn () => "Created new blockchain file\n");
               T.insert (!theTable) Chain.genesisHash (Nil 0);
               A.update (thePrimaryFork, 0, 0)
            end

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

   end
