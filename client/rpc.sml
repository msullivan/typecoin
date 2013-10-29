
structure RPC :> RPC =
   struct

      structure M = RpcMessage
      open RpcClient

      structure Blockchain =
         struct

            type hash = Bytestring.string
            type pos = Int64.int
      
            fun lastBlock () =
               (case rpc M.LastBlock of
                   M.Int i => i
                 | _ => raise RPC)
      
            
            fun positionByNumber i =
               (case rpc (M.PositionByNumber i) of
                   M.Integer i => Int64.fromLarge i
                 | _ => raise RPC)
      
            fun getTx hash =
               (case rpc (M.LookupTx hash) of
                   M.Transaction tx => SOME tx
                 | M.False => NONE
                 | _ => raise RPC)

         end

      structure Process =
         struct
      
            fun inject tx =
               (case rpc (M.Inject tx) of
                   M.True => ()
                 | _ => raise RPC)

         end
   end
