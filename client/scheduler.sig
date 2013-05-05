
signature SCHEDULER =
   sig

      val yield : unit -> 'a
      val shutdown : unit -> 'a

      (* sockets *)
      val insertRead : 'mode Network.sock -> (unit -> unit) -> unit
      val insertWrite : 'mode Network.sock -> (unit -> unit) -> unit
      val delete : 'mode Network.sock -> unit

      (* timeouts *)
      type tid
      val dummy : tid
      val onceAbs : Time.time -> (unit -> unit) -> tid
      val once : Time.time -> (unit -> unit) -> tid
      val repeating : Time.time -> (unit -> unit) -> tid
      val cancel : tid -> unit

      val numberOfTimeouts : unit -> int

      (* Starts the scheduler, passing control to the given continuation. *)
      val start : (unit -> unit) -> unit

   end
