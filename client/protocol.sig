
signature PROTOCOL =
   sig

      type ssbuf
      type sstream = Network.asock * ssbuf

      val recvMessage : sstream -> Message.message
      val close : sstream -> unit

      val connect : Network.addr -> sstream option
      val getblocks : sstream -> Message.inv list option
      val getdata : sstream -> Message.inv -> Message.message option
      
   end
