
signature RPC_CLIENT =
   sig

      exception RPC
      exception RemoteError of string

      val rpc : RpcMessage.request -> RpcMessage.response
      val close : unit -> unit
      val shutdown : unit -> unit

   end
