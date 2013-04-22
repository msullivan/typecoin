
signature SCHEDULER =
   sig

      val yield : unit -> 'a
      val shutdown : unit -> 'a

      val setTimeout : Time.time -> (unit -> unit) -> unit
      val insertSock : 'mode Network.sock -> (unit -> unit) -> unit
      val deleteSock : 'mode Network.sock -> unit

      val start : unit -> unit
      val cleanup : unit -> unit

   end
