
signature PLATFORM =
   sig

      val print : string -> unit
      val Socket_connect : ('af, 'sock_type) Socket.sock * 'af Socket.sock_addr -> unit
      val Socket_close : ('af, 'sock_type) Socket.sock -> unit

   end