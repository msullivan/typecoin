
signature PEER =
   sig

      val relayedPenalty : Time.time

      type peer

      val address : peer -> Address.addr
      val time : peer -> Time.time

      val new : Address.addr -> Time.time -> unit
      val update : peer -> Time.time -> unit
      val delete : peer -> unit

      val next : unit -> peer option
      val enqueue : peer -> unit

      val wantPeers : unit -> int
      val relayable : unit -> peer list

      val initialize : unit -> unit
      val maintenance : unit -> unit

   end
