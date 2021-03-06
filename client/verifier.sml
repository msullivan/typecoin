
structure Verifier :> VERIFIER =
   struct

      structure BS = Bytesubstring
      structure BC = RPC.Blockchain

      structure Pos = Int64
      structure MIO = 
         MultiFileIOFun
         (structure ImpIO = BinIO
          structure String = Bytestring
          structure SeekIO = BinSeekIO
          val pieceSize = 0x20000000
          val fileSize = OS.FileSys.fileSize
          structure Integer = Pos)


      val chunkSize = 4096

      fun inputCostring ins pos =
         let in
            MIO.SeekIO.seekIn (ins, pos);
            BytesubstringCostring.fromProcess (fn () => BS.full (MIO.inputN (ins, chunkSize)))
         end


      fun inputString ins pos sz =
         let in
            MIO.SeekIO.seekIn (ins, pos);
            MIO.inputN (ins, sz)
         end


      fun readTx ins pos =
         let
            val (tx, _) =
               Transaction.reader (inputCostring ins pos)
               handle Overflow => raise Reader.SyntaxError
         in
            tx
         end



      fun get i =
         SOME (BC.positionByNumber i)
         handle RPC.RPC => NONE
              | RPC.RemoteError err =>
                   (
                   Log.long (fn () => "Remote error: "^ err);
                   NONE
                   )

         
      val empty : int * Pos.int Queue.queue = (0, Queue.empty)

      (* buffer begins at i *)
      fun next i (buffer as (sz, _)) limit =
         let
            fun fill (buf as (sz, q)) =
               if sz >= 30 orelse i+sz > limit then
                  buf
               else
                  (case get (i+sz) of
                      NONE =>
                         buf
                    | SOME entry =>
                         fill (sz+1, Queue.insert q entry))

            val (bufsize, queue) =
               if sz mod 5 = 0 then
                  fill buffer
               else
                  buffer
         in
            if bufsize > 0 then
               let
                  val (entry, q') = Queue.front queue
               in
                  (entry, (bufsize-1, q'))
               end
            else
               (
               OS.Process.sleep (Time.fromSeconds 30);
               next i buffer limit
               )
         end


      fun saveUtxo utxo i =
         let
            val name =
               if #testnet (!Chain.theChain) then
                  "testutxo"
               else
                  "utxo"

            val path = OS.Path.concat (Constants.dataDirectory, "utxo" ^ Int.toString i)
                  
            val outs = BinIO.openOut path
         in
            Utxo.writeTables outs utxo;
            BinIO.closeOut outs
         end



      fun fileExists filename =
         (OS.FileSys.fileSize filename; true)
         handle OS.SysErr _ => false
              | Overflow => true

      val utxoinc = 5000

      (* load the latest utxo table not later than i, where i mod utxoinc = 0 *)
      fun bestUtxo i =
         if i = 0 then
            (Utxo.new (), 0)
         else
            let
               val path = OS.Path.concat (Constants.dataDirectory, "utxo" ^ Int.toString i)
            in
               if fileExists path then
                  let
                     val ins = BinIO.openIn path
                     val res = Utxo.readTables ins
                  in
                     BinIO.closeIn ins;
   
                     (case res of
                         SOME (utxo :: _) =>
                            (
                            Log.long (fn () => "UTXO table loaded at "^ Int.toString i);
                            (utxo, i)
                            )
                       | _ =>
                            bestUtxo (i - utxoinc))
                  end
               else
                  bestUtxo (i - utxoinc)
            end


      (* takes a utxo table incorporating up to i-1, and advances it to j *)
      (* This goes so fast, it's not worth buffering. *)
      fun advanceUtxo ins utxo i j =
         if i > j then
            ()
         else
            (case get i of
                SOME pos =>
                   let
                      val () = MIO.SeekIO.seekIn (ins, pos+36)
                      val sz = Word32.toInt (ConvertWord.bytesToWord32L (MIO.inputN (ins, 4)))
                      val blstr = MIO.inputN (ins, sz)
                   in
                      Utxo.processBlock utxo (pos+40) i blstr;
             
                      if i mod 1000 = 0 then
                         Log.long (fn () => "Re-indexed block " ^ Int.toString i)
                      else
                         () ;
       
                      if i mod utxoinc = 0 then
                         saveUtxo utxo i
                      else
                         () ;
                
                      advanceUtxo ins utxo (i+1) j
                   end
              | NONE =>
                   (
                   OS.Process.sleep (Time.fromSeconds 30);
                   advanceUtxo ins utxo i j
                   ))
                   


      (* returns a utxo table incorporating up to i *)
      fun seekUtxo ins i =
         let
            val (utxo, j) = bestUtxo (i - (i mod utxoinc))
         in
            advanceUtxo ins utxo (j+1) i;
            utxo
         end
      

      fun verify ins utxo i pos =
         let
            val () = MIO.SeekIO.seekIn (ins, pos+36)
            val sz = Word32.toInt (ConvertWord.bytesToWord32L (MIO.inputN (ins, 4)))
            val blstr = MIO.inputN (ins, sz)
         in
            Verify.verifyStoredBlock
               (fn utxo => fn hash =>
                   Option.map (readTx ins) (Utxo.find utxo hash))
               utxo 
               (pos+40)
               i
               (EBlock.fromBytes blstr)
         end


      (* buffer begins at i *)
      fun verifyLoop ins utxo buffer i j =
         if i > j then
            ()
         else 
            let
               val (pos, buffer') = next i buffer j
            in
               if verify ins utxo i pos then
                  (
                  Log.long (fn () => "Verified "^ Int.toString i );
      
                  if i mod utxoinc = 0 then
                     saveUtxo utxo i
                  else
                     () ;
      
                  verifyLoop ins utxo buffer' (i+1) j
                  )
               else
                  Log.long (fn () => "Verification failed on " ^ Int.toString i)
            end


      fun computeUtxo i =
         let
            val path = OS.Path.concat (Constants.dataDirectory, #blockchainFile (!Chain.theChain))
            val ins = MIO.openIn path
            val utxo = seekUtxo ins i
         in
            MIO.closeIn ins;
            utxo
         end


      fun verifier start finish logfile =
         if start < 1 orelse finish < start then
            ()
         else
            let
               val path = OS.Path.concat (Constants.dataDirectory, #blockchainFile (!Chain.theChain))
               val () = Log.initialize logfile
               val ins = MIO.openIn path
               val utxo = seekUtxo ins (start-1)
            in
               let in
                  verifyLoop ins utxo empty start finish;
                  RpcClient.close ();
                  MIO.closeIn ins;
                  Log.cleanup ()
               end
               handle exn =>
                  (
                  RpcClient.close ();
                  MIO.closeIn ins;
                  Log.cleanup ();
                  raise exn
                  )
            end
   
   end
