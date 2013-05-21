
signature PEER =
   sig

      type peer

      val address : peer -> Address.addr
      val time : peer -> Time.time

      val new : Address.addr -> peer
      val update : peer -> Time.time -> unit
      val delete : peer -> unit

      val insertMaybe : Address.addr -> Time.time -> unit  (* Create a new peer and update it, if peers are wanted. *)

      val next : int -> peer option  (* int = number of consecutive failures *)
      val enqueue : peer -> unit

      val wantPeers : unit -> int
      val relayable : unit -> (Time.time * Address.addr) list

      val initialize : unit -> unit  

   end
