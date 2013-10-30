
structure RPC :> RPC =
   struct

      structure M = RpcMessage
      open RpcClient

      structure Blockchain =
         struct

            type hash = Bytestring.string
            type pos = Int64.int
      
            fun member hash =
               (case rpc (M.BlockMember hash) of
                   M.True => true
                 | M.False => false
                 | _ => raise RPC)

            fun blockData hash =
               (case rpc (M.LookupBlock hash) of
                   M.String str => str
                 | _ => raise RPC)

            fun block hash =
               Block.readBlock (blockData hash)

            fun blockPrimary hash =
               (case rpc (M.BlockPrimary hash) of
                   M.True => true
                 | M.False => false
                 | _ => raise RPC)

            fun lastBlock () =
               (case rpc M.LastBlock of
                   M.Int i => i
                 | _ => raise RPC)

            fun totalDifficulty () =
               (case rpc M.TotalDifficulty of
                   M.Integer i => i
                 | _ => raise RPC)
      
            fun dataByNumber i =
               (case rpc (M.BlockByNumber i) of
                   M.String str => str
                 | _ => raise RPC)
      
            fun tx hash =
               (case rpc (M.LookupTx hash) of
                   M.Transaction tx => SOME tx
                 | M.False => NONE
                 | _ => raise RPC)

            fun txDataByNumber i j =
               (case rpc (M.TxByNumber (i, j)) of
                   M.String str => str
                 | _ => raise RPC)

            fun txByNumber i j =
               Transaction.readTx (txDataByNumber i j)

         end

      structure Process =
         struct
      
            fun inject tx =
               (case rpc (M.Inject tx) of
                   M.True => ()
                 | _ => raise RPC)

         end
   end
