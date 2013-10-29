
structure RpcAction :> RPC_ACTION =
   struct

      structure M = RpcMessage
   
      fun act req =
         (case req of
             M.CloseChannel =>
                raise (Fail "precondition")

           | M.ShutdownServer =>
                raise (Fail "precondition")

           | M.Inject tx =>
                (
                Process.inject tx;
                M.True
                )

           | M.LookupTx hash =>
                (case Blockchain.getTx hash of
                    SOME tx =>
                       M.Transaction tx
                  | NONE =>
                       M.False)

           | M.LastBlock =>
                M.Int (Blockchain.lastBlock ())

           | M.PositionByNumber i =>
                M.Integer (Int64.toLarge (Blockchain.positionByNumber i)))

   end

