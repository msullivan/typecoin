
signature COMMO =
   sig

      type conn

      val openConn : Peer.peer -> (conn -> unit) -> unit
      val sendMessage : conn -> Message.message -> bool

      val initialize : (conn * Message.message -> unit) -> unit

   end
