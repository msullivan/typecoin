
signature RPC_ACTION =
   sig

      (* Precondition: the request is not CloseChannel or ShutdownServer. *)
      val act : RpcMessage.request -> RpcMessage.response

   end
