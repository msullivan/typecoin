
signature PROTOCOL =
   sig

      type buffer
      type bufsock = Network.asock * buffer

      exception NoMessage
      val recvMessage : bufsock -> Message.message

      val handshake : Network.addr -> bufsock option
      
   end
