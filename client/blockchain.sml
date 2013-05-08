
structure Blockchain :> BLOCKCHAIN =
   struct

      structure A = Array
      structure B = Bytestring
      structure BS = Bytesubstring

      structure Pos = Int64
      structure MIO = 
         MultiFileIOFun
         (structure ImpIO = BinIO
          structure String = Bytestring
          structure SeekIO = BinSeekIO
          val pieceSize = 0x20000000
          val fileSize = OS.FileSys.fileSize
          structure Integer = Pos)


      (* Precomputed data *)
      val genesisRecord =
         B.concat [ConvertWord.word32ToBytesL 0w0,
                   Chain.genesisHash,
                   ConvertWord.word32ToBytesL (Word32.fromInt (B.size Chain.genesisBlock)),
                   Chain.genesisBlock]

      (* Codes *)
      val codeDeprecated : Word32.word = 0w1



      (* I/O *)

      type pos = Pos.int

      exception BlockchainIO

      val theOutstream : MIO.outstream option ref = ref NONE
      val theOutPos : Pos.int ref = ref 0
      val theInstream : MIO.instream option ref = ref NONE

      fun outputBlock hash blstr =
         let
            val pos = !theOutPos
            val outs = valOf (!theOutstream)
            val sz = B.size blstr
         in
            MIO.output (outs, ConvertWord.word32ToBytesL 0w0);
            MIO.output (outs, hash);
            MIO.output (outs, ConvertWord.word32ToBytesL (Word32.fromInt sz));
            MIO.output (outs, blstr);
            MIO.flushOut outs;
            theOutPos := pos + 40 + Pos.fromInt sz;
            pos
         end

      fun inputCode pos =
         let
            val ins = valOf (!theInstream)
         in
            MIO.SeekIO.seekIn (ins, pos);
            ConvertWord.bytesToWord32L (MIO.inputN (ins, 4))
         end

      fun inputHash pos =
         let
            val ins = valOf (!theInstream)
         in
            MIO.SeekIO.seekIn (ins, pos+4);
            MIO.inputN (ins, 32)
         end

      fun inputData pos =
         let
            val ins = valOf (!theInstream)
            val () = MIO.SeekIO.seekIn (ins, pos+36)
            val sz = Word32.toInt (ConvertWord.bytesToWord32L (MIO.inputN (ins, 4)))
         in
            MIO.inputN (ins, sz)
         end

      fun inputDiffBits pos =
         let
            val ins = valOf (!theInstream)
         in
            MIO.SeekIO.seekIn (ins, pos+112);
            ConvertWord.bytesToWord32L (MIO.inputN (ins, 4))
         end

      fun outputDeprecate pos =
         let
            val code = inputCode pos
            val code' = Word32.orb (code, codeDeprecated)

            val path = OS.Path.concat (Constants.dataDirectory, Chain.blockchainFile)
            val outs = MIO.openAppend path
         in
            MIO.SeekIO.seekOut (outs, pos);
            MIO.output (outs, ConvertWord.word32ToBytesL code');
            MIO.closeOut outs
         end


      fun fileExists filename =
         (OS.FileSys.fileSize filename; true)
         handle OS.SysErr _ => false
              | Overflow => true




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


      (* The blockchain is a tree that looks like this:

         ... === 100 === 101 === 102 === 103 === 104 === 105 === ...
                   \               \
                    \               \--- 103c -- 104c
                     \
                      \- 101a -- 102a -- 103a
                           \
                            \--- 102b

         (Nearly always the tree will be much less bushy than this.  Only very rarely will a fork
         have even two blocks.)  We call the top branch (..., 100, 101, ..., 105, ...) the primary
         fork.  Every other block in the picture is on a secondary fork.

         Conceptually this is a tree, but we don't want to represent it that way; nearly every
         block will be on the main primary fork, so it's wasteful to used a linked data structure.
         Also, a linked data structure won't give us a good way to determine whether a block is on
         the primary fork.  Instead, we put the primary fork into an array, and used linked data
         structures only for the secondary forks, which will be very sparse.

         On occasion, a secondary fork will grow so that its cumulative difficulty is greater than
         that of the primary fork.  When that happens, the secondary fork becomes the new primary
         fork and the old primary fork becomes a secondary fork.  Then we must reorganize the data
         structure to move the new primary fork into the array, and move the old primary fork into
         linked data structures.

         A *lineage* gives the position of a block in the blockchain.  For a block on the primary
         fork, the lineage is "Nil num", where num is the block's index in the chain.  (Whatever
         else we might wish to know can be recovered from the index.)  For a block on a secondary
         fork, the lineage is "Cons (pos, num, cumdiff, predlin)", where:

         - pos is the block's position in the record
         - num is the block's number
         - cumdiff is the cumulative difficulty of the branch ending in that block
         - predlin is a reference to the block's predecessor's lineage

         In most cases we don't work with lineages, but references to lineages, because sometimes
         a block will be moved to or from the primary fork without any action to the block's
         successors.  For example, 101a-102a-103a might be moved onto the primary fork while 102b
         stays on a secondary fork.  Conversely, 102 might be moved off the primary fork while 103c
         stays off.

         Our data structure has five pieces:

         1. The record is a file containing all the blocks on any fork.  It is accessed through
            theInstream and theOutstream.  The current position of theOutstream in the file is
            maintained in theOutPos.

         2. lastblock       holds the block number of the primary fork's head
         3. totaldiff       holds the cumulative difficulty to the primary fork's head
         4. theTable        is a hash table mapping block hashes to their lineage reference
         5. thePrimaryFork  is an array mapping primary-fork block numbers to each block's position
                            in the record

         (I'm a bit concerned about the scalability of totaldiff.  If the Bitcoin difficulty grows
         exponentially (which Moore's law suggests is likely), totaldiff will grow exponentially, so
         the representation of totaldiff will grow linearly.)

         Orphans
         -------
         We don't store orphans in the main data structure at all, and they don't appear in the
         picture.  Instead, each connection has an orphanage containing orphans received from that
         connection.  If we eventually receive an orphan's predecessor from that connection, the
         orphan will automatically be linked into the tree.  If not, the orphanage will be gc'ed
         away when the connection closes.

         We probably wouldn't bother to do this, except that the blockchain download protocol
         depends on it.
      *)

      datatype lineage =
         (* block is on the primary fork: block number *)
         Nil of int

         (* block is on a secondary fork:
            position, block number, cumulative difficulty, predecessor's lineage
         *)
       | Cons of pos * int * IntInf.int * lineage ref


      val lastblock = ref 0
      val totaldiff : IntInf.int ref = ref 0
      val theTable : lineage ref T.table = T.table Constants.blockTableSize
      val thePrimaryFork : pos A.array = A.array (Constants.primaryForkSize, 0)

      type orphanage = B.string T.table * hash T.table  (* orphans, orphans' predecessors *)



      (* The operation "rewind num" shunts every block on the primary fork back to (but not including)
         num onto a secondary fork.
      *)
      fun rewind backto =
         let
            fun loop () =
               if !lastblock = backto then
                  let
                     val pos = A.sub (thePrimaryFork, !lastblock)
                     val hash = inputHash pos
                  in
                     T.lookup theTable hash
                  end
               else
                  let
                     val num = !lastblock
                     val pos = A.sub (thePrimaryFork, num)
                     val cumdiff = !totaldiff
                     val diff = Verify.decodeDifficulty (inputDiffBits pos)

                     val () = lastblock := num - 1
                     val () = totaldiff := cumdiff - diff

                     val predlinr = loop ()

                     val hash = inputHash pos
                     val lineager = T.lookup theTable hash
                  in
                     lineager := Cons (pos, num, cumdiff, predlinr);
                     lineager
                  end
         in
            loop ()
         end


      (* Puts lineage on the primary fork, rewinding what is currently there to make room for it. *)
      fun setPrimary lineage =
         let
            fun loop lineage =
               (case lineage of
                   Nil num =>
                      let in
                         rewind num;
                         ()
                      end
                 | Cons (pos, num, cumdiff, predlin) =>
                      let 
                         val () = loop (!predlin)

                         val hash = inputHash pos
                         val lineager = T.lookup theTable hash
                      in
                         lineager := Nil num;
                         Array.update (thePrimaryFork, num, pos);
                         lastblock := num;
                         totaldiff := cumdiff
                      end)
         in
            loop lineage
         end


      (* "cumulativeDifficulty num" computes the cumulative difficulty of the primary fork up to the
         block numbered num.
      *)
      fun cumulativeDifficulty upto =
         let
            (* cumulative difficulty up to num is diff *)
            fun loop num diff =
               if num = upto then
                  diff
               else
                  loop (num-1) (diff - Verify.decodeDifficulty (inputDiffBits (A.sub (thePrimaryFork, num))))
         in
            loop (!lastblock) (!totaldiff)
         end


      fun deprecateByNumber _ = ()


      datatype result = ORPHAN | NOEXTEND | EXTEND

      datatype mode = RECEIVE | LOAD of pos

      fun insertBlockMain (orphanTable, orphanPredTable) hash blstr mode verifyit =
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
                      (* Previous block is not in the table.  This is an orphan block. *)
                      (case mode of
                          LOAD _ =>
                             ORPHAN
                        | _ =>
                             (* Store received orphans. *)
                             (
                             T.insert orphanTable hash blstr;
                             T.insert orphanPredTable prev hash;
                             ORPHAN
                             ))

                 | SOME predlinr =>
                      let
                         val predlin = !predlinr

                         val pos =
                            (* If we're loading, use the given position.  Otherwise write to file. *)
                            (case mode of
                                LOAD pos =>
                                   pos
                              | _ =>
                                   outputBlock hash blstr)

                         val num =
                            1 + (case predlin of
                                    Nil n => n
                                  | Cons (_, n, _, _) => n)
    
                         val diff =
                            Verify.decodeDifficulty (ConvertWord.bytesToWord32L (B.substring (blstr, 72, 4)))
                            +
                            (case predlin of
                                Nil n =>
                                   cumulativeDifficulty n
                              | Cons (_, _, preddiff, _) =>
                                   preddiff)
                      in
                         if diff > !totaldiff then
                            (* Extending the longest chain. *)
                            let in
                               if num > !lastblock then
                                  ()
                               else
                                  (* We are extending the longest chain difficulty-wise, but not height-wise. *)
                                  Log.long (fn () => "Fork detected at " ^ B.toStringHex (B.rev hash));

                               setPrimary predlin;
                               Array.update (thePrimaryFork, num, pos);
                               T.insert theTable hash (ref (Nil num));
                               lastblock := num;
                               totaldiff := diff;
   
                               if verifyit andalso not (Verify.verifyBlock blstr) then
                                  let in
                                     Log.long (fn () => "Dubious block detected at " ^ B.toStringHex (B.rev hash));
                                     deprecateByNumber num
                                  end
                               else
                                  ();
   
                               EXTEND
                            end
                         else
                            (* On a secondary fork. *)
                            let 
                               val deprecated = verifyit andalso not (Verify.verifyBlock blstr)
                               (* XX use deprecated *)

                               val lineage = Cons (pos, num, diff, predlinr)
                            in
                               T.insert theTable hash (ref lineage);
                               
                               Log.long (fn () => "Fork detected at " ^ B.toStringHex (B.rev hash));

                               if deprecated then
                                  Log.long (fn () => "Dubious block detected at " ^ B.toStringHex (B.rev hash))
                               else
                                  ();
   
                               NOEXTEND
                            end
                      end)
            end


      fun insertBlock (orphanage as (orphanTable, orphanPredTable)) hash blstr verifyit =
         let
            val result = insertBlockMain orphanage hash blstr RECEIVE verifyit
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
       
                             insertBlockMain orphanage hash' blstr' RECEIVE verifyit
                             (* Our result is just the result for hash', because we are extending
                                the longest fork iff hash' extends the longest fork.
                                (The result for hash' can't be ORPHAN, since we just inserted its
                                predecesor (ie, hash).)
                             *)
                          end
                     | NONE =>
                          result))
         end


      val dummyOrphanage : orphanage = (T.table 1, T.table 1)

      fun loadBlock hash blstr pos =
         (case insertBlockMain dummyOrphanage hash blstr (LOAD pos) false of
             EXTEND =>
                if !lastblock mod 1000 = 0 then
                   Log.long (fn () => "Loaded block " ^ Int.toString (!lastblock))
                else
                   ()
           | _ => ())

      fun loadFile pos instream =
         let
            val code = MIO.inputN (instream, 4)
         in
            if B.size code = 0 then
               pos
            else if B.size code <> 4 then
               let in
                  Log.long (fn () => "File ends with incomplete record");
                  pos
               end
            else
               let
                  val hash = MIO.inputN (instream, 32)
               in
                  if B.size hash <> 32 then
                     let in
                        Log.long (fn () => "File ends with incomplete record");
                        pos
                     end
                  else
                     let
                        val szstr = MIO.inputN (instream, 4)
                     in
                        if B.size szstr <> 4 then
                           let in
                              Log.long (fn () => "File ends with incomplete record");
                              pos
                           end
                        else
                           let
                              val sz =
                                 Word32.toInt (ConvertWord.bytesToWord32L szstr)
            
                              val blstr = MIO.inputN (instream, sz)
                           in
                              if B.size blstr <> sz then
                                 let in
                                    Log.long (fn () => "File ends with incomplete record");
                                    pos
                                 end
                              else 
                                 let in
                                    if Word32.andb (ConvertWord.bytesToWord32L code, codeDeprecated) = 0w0 then
                                       loadBlock hash blstr pos
                                    else
                                       ();

                                    loadFile (pos + 40 + Pos.fromInt sz) instream
                                 end
                           end
                     end
               end
         end


      
      fun initialize () =
         let
            val path = OS.Path.concat (Constants.dataDirectory, Chain.blockchainFile)
         in
            lastblock := 0;
            T.reset theTable Constants.blockTableSize;
            T.insert theTable Chain.genesisHash (ref (Nil 0));
            A.update (thePrimaryFork, 0, 0);
   
            if MIO.exists path then
               (* Load the blockchain record:
                  We need two instreams: one for the load (instream), the other for random access
                  (!theInstream) to look up information about old blocks.  We could make do with
                  just one and seek it back and forth, but that would be expensive and a pain.
                  Usually there will be very little random access; nearly all reading during the
                  load will be sequential.
               *)
               let
                  val instream = MIO.openIn path
               in
                  (* Verify that the first record looks right. *)
                  if B.eq (MIO.inputN (instream, B.size genesisRecord), genesisRecord) then
                     ()
                  else
                     (
                     MIO.closeIn instream;
                     raise BlockchainIO
                     );
   
                  theInstream := SOME (MIO.openIn path);
   
                  Log.long (fn () => "Loading blockchain file");
                  let
                     val pos = loadFile (Pos.fromInt (B.size genesisRecord)) instream
                  in
                     theOutPos := pos;
                     MIO.closeIn instream;
   
                     (* Open the outstream and seek it to pos.  Usually pos will be the end,
                        where it opens anyway, but it might be different if there the file has
                        an incomplete record at the end.
                     *)
                     theOutstream := SOME (MIO.openAppend path);
                     MIO.SeekIO.seekOut (valOf (!theOutstream), pos);
   
                     Log.long (fn () => "Load complete at block " ^ Int.toString (!lastblock));
                     Log.long (fn () => "Total difficulty " ^ IntInf.toString (!totaldiff))
                  end
               end
            else
               (* Start a new blockchain record. *)
               let
                  val outstream = MIO.openAppend path
               in
                  theOutstream := SOME outstream;
                  theInstream := SOME (MIO.openIn path);
      
                  MIO.output (outstream, genesisRecord);
                  MIO.flushOut outstream;
                  theOutPos := Pos.fromInt (B.size genesisRecord);
                  Log.long (fn () => "Created new blockchain file")
               end
         end

      fun close () =
         (
         Option.app MIO.closeOut (!theOutstream);
         Option.app MIO.closeIn (!theInstream)
         )



      exception Absent = T.Absent

      fun member hash = T.member theTable hash

      fun blockPosition hash =
         (case !(T.lookup theTable hash) of
             Nil num =>
                A.sub (thePrimaryFork, num)
           | Cons (pos, _, _, _) => pos)

      fun blockNumber hash =
         (case !(T.lookup theTable hash) of
             Nil num => num
           | Cons (_, num, _, _) => num)

      fun blockData hash = inputData (blockPosition hash)

      fun lastBlock () = !lastblock

      fun totalDifficulty () = !totaldiff

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
         (T.table Constants.orphanTableSize, T.table Constants.orphanTableSize)

      fun orphanageMember (orphanTable, _) hash = T.member orphanTable hash

      fun orphanageSize (orphanTable, _) = T.size orphanTable

   end
