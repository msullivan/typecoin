
structure Blockchain :> BLOCKCHAIN =
   struct

      (* Constants *)
      val tableSize = 0x100000
      val primaryForkSize = 0x080000

      val orphanTableSize = 7


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
      val theOutPos : Position.int ref = ref 0
      val theRainstream = ref RAIO.dummyIn

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
            val pos = !theOutPos
            val outs = !theOutstream
            val sz = B.size blstr
         in
            BinIO.output (outs, hash);
            BinIO.output (outs, ConvertWord.word32ToBytesL (Word32.fromInt sz));
            BinIO.output (outs, blstr);
            BinIO.flushOut outs;
            theOutPos := pos + 36 + sz;
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


      type orphanage = B.string T.table * hash T.table  (* orphans, orphans' predecessors *)

      datatype lineage =
         (* block is on the primary fork: block number *)
         Nil of int

         (* block is on a secondary fork:
            position, block number, predecessor, predecessor's lineage
         *)
       | Cons of pos * int * hash * lineage


      val lastblock = ref 0
      val lasthash = ref Chain.genesisHash
      val theTable : lineage T.table = T.table tableSize
      val thePrimaryFork : pos A.array = A.array (primaryForkSize, 0)



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
                   T.insert theTable hash (Nil num);
                   T.insert theTable oldhash oldlin;
                   (oldhash, oldlin)
                end)


      datatype result = ORPHAN | NOEXTEND | EXTEND

      fun insertBlockMain (orphanTable, orphanPredTable) hash blstr poso =
         if T.member theTable hash then
            NOEXTEND
         else if T.member orphanTable hash then
            ORPHAN
         else
            let
               val prev = B.substring (blstr, 4, 32)
            in
               (case T.find theTable prev of
                   NONE =>
                      (* Previous block is not in the table. This is an orphan block. *)
                      (case poso of
                          SOME _ =>
                             (* Shouldn't see orphans when loading, but if we do, ignore them. *)
                             ORPHAN
                        | NONE =>
                             (
                             T.insert orphanTable hash blstr;
                             T.insert orphanPredTable prev hash;
                             ORPHAN
                             ))
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
                            makePrimary prev lineage;
                            Array.update (thePrimaryFork, num, pos);
                            T.insert theTable hash (Nil num);
                            lastblock := num;
                            lasthash := hash;
                            EXTEND
                            )
                         else
                            (* On a secondary fork. *)
                            (
                            T.insert theTable hash (Cons (pos, num, prev, lineage));
                            
                            (case lineage of
                                Nil _ => Log.long (fn () => "Fork detected at " ^ B.toStringHex (B.rev hash))
                              | _ => ());

                            NOEXTEND
                            )
                      end)
            end


      fun insertBlock (orphanage as (orphanTable, orphanPredTable)) hash blstr =
         let
            val result = insertBlockMain orphanage hash blstr NONE
         in
            (case result of
                ORPHAN => ORPHAN
              | _ =>
                   (* hash is not an orphan; check whether hash is the predecessor to an orphan,
                      If so, insert the successor too.
                   *)
                   (case T.find orphanPredTable hash of
                       SOME hash' =>
                          let
                             val blstr' = T.lookup orphanTable hash'
                          in
                             T.remove orphanPredTable hash;
                             T.remove orphanTable hash';
       
                             insertBlockMain orphanage hash' blstr' NONE
                             (* Our result is the result for hash'.  It can't be ORPHAN, since we just
                                inserted its predecesor (ie, hash), and if it's EXTENDS we want it.
                             *)
                          end
                     | NONE =>
                          result))
         end


      val dummyOrphanage : orphanage = (T.table 1, T.table 1)

      fun loadBlock hash blstr pos =
         (case insertBlockMain dummyOrphanage hash blstr (SOME pos) of
             EXTEND =>
                if !lastblock mod 10000 = 0 then
                   Log.long (fn () => "Loaded block " ^ Int.toString (!lastblock))
                else
                   ()
           | _ => ())

      fun loadFile pos instream =
         let
            val hash = BinIO.inputN (instream, 32)
         in
            if B.size hash = 0 then
               (
               BinIO.closeIn instream;
               theOutPos := pos
               )
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
                     loadFile (pos+36+sz) instream
                     )
                  else
                     raise BlockchainIO
               end
         end

      fun initialize () =
         (
         lastblock := 0;
         lasthash := Chain.genesisHash;
         T.reset theTable;

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

               T.insert theTable Chain.genesisHash (Nil 0);
               A.update (thePrimaryFork, 0, 0);

               Log.long (fn () => "Loading blockchain file");
               loadFile (B.size genesisRecord) instream;
               Log.long (fn () => "Load complete at block " ^ Int.toString (!lastblock))
            end
         else
            (* start a new blockchain record *)
            let
               val outstream = BinIO.openOut "blockchain"
            in
               theOutstream := outstream;
               theRainstream := RAIO.fromInstream (BinIO.openIn "blockchain");
               theOutPos := 0;
   
               BinIO.output (outstream, genesisRecord);
               BinIO.flushOut outstream;
               Log.long (fn () => "Created new blockchain file");
               T.insert theTable Chain.genesisHash (Nil 0);
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

      fun member hash = T.member theTable hash

      fun blockPosition hash =
         (case T.lookup theTable hash of
             Nil num =>
                A.sub (thePrimaryFork, num)
           | Cons (pos, _, _, _) => pos)

      fun blockNumber hash =
         (case T.lookup theTable hash of
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



      fun newOrphanage () =
         (T.table orphanTableSize, T.table orphanTableSize)

      fun orphanageMember (orphanTable, _) hash = T.member orphanTable hash

      fun orphanageSize (orphanTable, _) = T.size orphanTable

   end
