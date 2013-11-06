
structure Verifier =
   struct

      structure BC = RPC.Blockchain

      fun retry f x =
         (f x handle RPC.RPC => (Log.long (fn () => ""); retry f x))
         

      (* takes a utxo table incorporating up to i, and advances it to j *)
      fun advanceUtxo utxo i j =
         if i > j then
            ()
         else
            (
            Utxo.processBlock utxo (retry BC.positionByNumber i + BC.blockOffsetInRecord) i (retry BC.dataByNumber i);
      
            if i mod 1000 = 0 then
               Log.long (fn () => "Re-indexed block " ^ Int.toString i)
            else
               () ;
      
            advanceUtxo utxo (i+1) j
            )

      fun verify utxo i =
         Verify.verifyStoredBlock
            (fn utxo => fn hash =>
                Option.map (retry BC.txByPosition) (Utxo.find utxo hash))
            utxo 
            (retry BC.positionByNumber i + BC.blockOffsetInRecord) 
            i
            (EBlock.fromBytes (retry BC.dataByNumber i))

      fun verifyLoop utxo i j =
         if i > j then
            ()
         else if verify utxo i then
            (
            Log.long (fn () => "Verified "^ Int.toString i );
            verifyLoop utxo (i+1) j
            )
         else
            Log.long (fn () => "Verification failed on " ^ Int.toString i)

      fun verifier start finish logfile =
         if start < 1 orelse finish < start then
            ()
         else
            let
               val utxo = Utxo.new ()
            in
               Log.initialize logfile;
               advanceUtxo utxo 1 (start-1);
               verifyLoop utxo start finish;
               RpcClient.close ();
               Log.cleanup ()
            end
   
   end
