
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
            theOutPos := pos + 36 + Position.fromInt sz;
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


      (* Since the blockchain is really a tree with very few limbs off the main trunk, the most
         natural way to represented it is as a tree with each node pointing to its predecessor.
         That is, as a bunch of lists (where the tree's branching arises by multiple list nodes
         sharing the same tail).  However, this won't do, for two related reasons: (1) It's
         wasteful, since almost all the nodes lie on the (current) primary fork, and (2) it
         doesn't give us a good way of determining whether a node is on the primary fork or a
         secondary one.

         Instead, we represent the blockchain using an array containing what is currently believed
         to be the primary fork.  Element i of the array gives the position (in the record) of the
         ith block.  When we look up a hash in the table, we are given a lineage.  A lineage is
         either (Nil num) -- indicating that the block is #num on the main fork -- or it is
         (Cons (pos, num, pred, predlin)) -- indicating that the block is on a secondary fork.  In
         the latter case, pos is the position (in the record) of the block, num is the number the
         block would be if it were on the main form, pred is the predecessor block's hash, and
         predlin is the predecessor's lineage.

         Thus, if a block has lineage (Cons (_, _, _, Nil _)) it is the first block on a fork.  Only
         very rarely will a fork contain even two blocks.

         Note that a secondary fork can become the primary fork it it becomes longer.  Then the
         array and lineages must be adjusted accordingly.
      *)

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
                      (* Previous block is not in the table.  This is an orphan block.

                         We don't store orphans in the main data structure at all.  Instead, each
                         connection has an orphanage containing orphans received from that connection.
                         If we eventually receive an orphan's predecessor from that connection,
                         the orphan will automatically be linked in.  Otherwise, the orphanage will
                         be gc'ed away when the connection closes.

                         We wouldn't bother to do this, except that the blockchain download protocol
                         depends on it.
                      *)
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
                                Nil _ =>
                                   (case poso of
                                       NONE =>
                                          Log.long (fn () => "Fork detected at " ^ B.toStringHex (B.rev hash))
                                     | SOME _ => ())
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
                if !lastblock mod 1000 = 0 then
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
                     loadFile (pos + 36 + Position.fromInt sz) instream
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

         if fileExists Chain.blockchainFile then
            let
               val instream = BinIO.openIn Chain.blockchainFile
               val gensz = B.size Chain.genesisBlock
            in
               theOutstream := BinIO.openAppend Chain.blockchainFile;
               theRainstream := RAIO.fromInstream (BinIO.openIn Chain.blockchainFile);

               (* Verify that the first record looks right. *)
               if B.eq (BinIO.inputN (instream, B.size genesisRecord), genesisRecord) then
                  ()
               else
                  raise BlockchainIO;

               T.insert theTable Chain.genesisHash (Nil 0);
               A.update (thePrimaryFork, 0, 0);

               Log.long (fn () => "Loading blockchain file");
               loadFile (Position.fromInt (B.size genesisRecord)) instream;
               Log.long (fn () => "Load complete at block " ^ Int.toString (!lastblock))
            end
         else
            (* start a new blockchain record *)
            let
               val outstream = BinIO.openOut Chain.blockchainFile
            in
               theOutstream := outstream;
               theRainstream := RAIO.fromInstream (BinIO.openIn Chain.blockchainFile);
   
               BinIO.output (outstream, genesisRecord);
               BinIO.flushOut outstream;
               theOutPos := Position.fromInt (B.size genesisRecord);
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
