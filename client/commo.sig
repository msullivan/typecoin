
signature COMMO =
   sig

      type peer

      val initialize : unit -> (peer * Message.message) CML.chan
      val cleanup : unit -> unit

      val addPeer : NetHostDB.in_addr -> peer option
      val sendMessage : peer -> Message.message -> bool

   end
