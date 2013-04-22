
signature COMMO =
   sig

      type peer

      val initialize : (peer * Message.message -> unit) -> unit

      val addPeer : NetHostDB.in_addr -> (peer -> unit) -> unit
      val sendMessage : peer -> Message.message -> bool

   end
