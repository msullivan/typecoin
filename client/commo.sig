
signature COMMO =
   sig

      type conn

      val openConns : (conn -> unit) -> unit
      val sendMessage : conn -> Message.message -> unit
      val closeConn : conn -> bool -> unit                (* bool=true if closed "with prejudice" *)

      val eq : conn * conn -> bool
      val lastBlock : conn -> int
      val orphanage : conn -> Blockchain.orphanage

      val numberOfConnections : unit -> int

      val initialize : (conn * Message.message -> unit) -> unit

   end
