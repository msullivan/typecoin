
signature SCHEDULER =
   sig

      val yield : unit -> 'a
      val shutdown : unit -> 'a

      val setTimeout : Time.time -> (unit -> unit) -> unit
      val insertRead : 'mode Network.sock -> (unit -> unit) -> unit
      val insertWrite : 'mode Network.sock -> (unit -> unit) -> unit
      val delete : 'mode Network.sock -> unit

      (* Starts the scheduler, passing control to the given continuation. *)
      val start : (unit -> unit) -> unit

   end
