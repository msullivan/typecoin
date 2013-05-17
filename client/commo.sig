
signature COMMO =
   sig

      type conn
      type state

      val sendMessage : conn -> Message.message -> unit
      val broadcastMessage : Message.message -> unit

      val closeConn : conn -> bool -> unit                (* bool=true if closed "with prejudice" *)
      val closed : conn -> bool

      val suspendPolling : unit -> unit
      val resumePolling : unit -> unit

      val eq : conn * conn -> bool
      val lastBlock : conn -> int
      val state : conn -> state

      val numberOfConnections : unit -> int

      (* first callback is for new connections, second is for messages over existing ones *)
      val initialize : (conn -> unit) -> (conn * Message.message -> unit) -> unit

   end
