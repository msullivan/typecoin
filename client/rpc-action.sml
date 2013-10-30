
structure RpcAction :> RPC_ACTION =
   struct

      structure M = RpcMessage
   
      fun act req =
         (case req of
             M.CloseChannel =>
                raise (Fail "precondition")

           | M.ShutdownServer =>
                raise (Fail "precondition")

           | M.BlockMember hash =>
                if Blockchain.member hash then
                   M.True
                else
                   M.False

           | M.LookupBlock hash =>
                M.String (Blockchain.blockData hash)

           | M.BlockPrimary hash =>
                if Blockchain.blockPrimary hash then
                   M.True
                else
                   M.False

           | M.LastBlock =>
                M.Int (Blockchain.lastBlock ())

           | M.TotalDifficulty =>
                M.Integer (Blockchain.totalDifficulty ())

           | M.BlockByNumber i =>
                M.String (Blockchain.dataByNumber i)

           | M.LookupTx hash =>
                (case Blockchain.tx hash of
                    SOME tx =>
                       M.Transaction tx
                  | NONE =>
                       M.False)

           | M.TxByNumber (i, j) =>
                M.String (Blockchain.txDataByNumber i j)

           | M.Inject tx =>
                (
                Process.inject tx;
                M.True
                ))

   end

