
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
         (Cons (pos, num, predlin)) -- indicating that the block is on a secondary fork.  In
         the latter case, pos is the position (in the record) of the block, num is the number the
         block would be if it were on the main form, and predlin is the predecessor's lineage.
         Predlin is a reference, because the predecessor might be moved onto or off the primary
         fork.

         Thus, if a block has lineage (Cons (_, _, ref (Nil _))) it is the first block on a fork.
         Only very rarely will a fork contain even two blocks.

         Note that a secondary fork can become the primary fork it it becomes longer.  Then the
         array and lineages must be adjusted accordingly.
      *)

      datatype lineage =
         (* block is on the primary fork: block number *)
         Nil of int

         (* block is on a secondary fork:
            position, block number, predecessor's lineage
         *)
       | Cons of pos * int * lineage ref


      val lastblock = ref 0
      val theTable : lineage ref T.table = T.table Constants.blockTableSize
      val thePrimaryFork : pos A.array = A.array (Constants.primaryForkSize, 0)



      (* Rewinds the primary fork to backto, moving the current contents to a secondary fork. *)
      fun rewind backto =
         let
            fun loop num =
               if num = backto then
                  let
                     val pos = A.sub (thePrimaryFork, num)
                     val hash = inputHash pos
                  in
                     T.lookup theTable hash
                  end
               else
                  let
                     val predlinr = loop (num-1)
                     val pos = A.sub (thePrimaryFork, num)
                     val hash = inputHash pos
                     val lineager = T.lookup theTable hash
                  in
                     lineager := Cons (pos, num, predlinr);
                     lineager
                  end
         in
            loop (!lastblock);
            lastblock := backto
         end


      (* Puts lineage on the primary fork, displacing what is currently there. *)
      fun setPrimary lineage =
         let
            fun loop lineage =
               (case lineage of
                   Nil num =>
                      rewind num
                 | Cons (pos, num, predlin) =>
                      let 
                         val () = loop (!predlin)
                         val hash = inputHash pos
                         val lineager = T.lookup theTable hash
                      in
                         lineager := Nil num;
                         Array.update (thePrimaryFork, num, pos)
                      end)
         in
            loop lineage;
            lastblock := (case lineage of
                             Nil num => num
                           | Cons (_, num, _) => num)
         end


      fun deprecateByNumber _ = ()


      datatype result = ORPHAN | NOEXTEND | EXTEND

      datatype mode =
         RECEIVE
       | LOAD of pos

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
                      (* Previous block is not in the table.  This is an orphan block.

                         We don't store orphans in the main data structure at all.  Instead, each
                         connection has an orphanage containing orphans received from that connection.
                         If we eventually receive an orphan's predecessor from that connection,
                         the orphan will automatically be linked in.  Otherwise, the orphanage will
                         be gc'ed away when the connection closes.

                         We probably wouldn't bother to do this, except that the blockchain download
                         protocol depends on it.
                      *)
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
                                  | Cons (_, n, _) => n)
                      in
                         if num > !lastblock then
                            (* Extending the longest chain. *)
                            (
                            setPrimary predlin;
                            Array.update (thePrimaryFork, num, pos);
                            T.insert theTable hash (ref (Nil num));
                            lastblock := num;

                            if verifyit andalso not (Verify.verifyBlock blstr) then
                               let in
                                  Log.long (fn () => "Dubious block detected at " ^ B.toStringHex (B.rev hash));
                                  deprecateByNumber num
                               end
                            else
                               ();

                            EXTEND
                            )
                         else
                            (* On a secondary fork. *)
                            let 
                               val deprecated = verifyit andalso not (Verify.verifyBlock blstr)
                               (* XX use deprecated *)

                               val lineage = Cons (pos, num, predlinr)
                            in
                               T.insert theTable hash (ref lineage);
                               
                               (case predlin of
                                   Nil _ =>
                                      Log.long (fn () => "Fork detected at " ^ B.toStringHex (B.rev hash))
                                 | _ => ());

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
                             (* Our result is the result for hash'.  It can't be ORPHAN, since we just
                                inserted its predecesor (ie, hash), and if it's EXTENDS we want it.
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
                     Log.long (fn () => "Loaded");
                     theOutPos := pos;
                     MIO.closeIn instream;
   
                     (* Open the outstream and seek it to pos.  Usually pos will be the end,
                        where it opens anyway, but it might be different if there the file has
                        an incomplete record at the end.
                     *)
                     theOutstream := SOME (MIO.openAppend path);
                     MIO.SeekIO.seekOut (valOf (!theOutstream), pos);
   
                     Log.long (fn () => "Load complete at block " ^ Int.toString (!lastblock))
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
           | Cons (pos, _, _) => pos)

      fun blockNumber hash =
         (case !(T.lookup theTable hash) of
             Nil num => num
           | Cons (_, num, _) => num)

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



      fun newOrphanage () =
         (T.table Constants.orphanTableSize, T.table Constants.orphanTableSize)

      fun orphanageMember (orphanTable, _) hash = T.member orphanTable hash

      fun orphanageSize (orphanTable, _) = T.size orphanTable

   end
