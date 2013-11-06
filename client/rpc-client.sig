
signature RPC_CLIENT =
   sig

      exception RPC
      exception RemoteError of string

      val rpc : Word8.word * Unityped.unityped -> Unityped.unityped
      val close : unit -> unit
      val shutdown : unit -> unit

   end
