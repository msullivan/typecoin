
structure Scheduler :> SCHEDULER =
   struct

      structure S = Socket
      val sameDesc = Platform.Socket_sameDesc

      datatype directive = YIELD | SHUTDOWN

      exception Yield
      exception Shutdown

      fun yield () = raise Yield
      fun shutdown () = raise Shutdown


      fun dispatch f =
         (f (); YIELD)
         handle Yield => YIELD
              | Shutdown => SHUTDOWN
         (* Should we catch exceptions here? *)


      val socks : S.sock_desc list ref = ref []
      val callbacks : (S.sock_desc * (unit -> unit)) list ref = ref []
      val timeout = ref Time.zeroTime
      val timecall : (unit -> unit) ref = ref (fn () => raise Shutdown)

      fun cleanup () =
         (
         socks := [];
         callbacks := [];
         timeout := Time.zeroTime;
         timecall := (fn () => raise Shutdown)
         )

      fun start () =
         (case S.select {rds=(!socks), wrs=[], exs=[], timeout=SOME (!timeout)} of
             {rds=[], ...} =>
                (case dispatch (!timecall) of
                    SHUTDOWN => ()
                  | YIELD => start ())
           | {rds=ready, ...} =>
                let
                   (* Note that if a socket is deleted after it becomes ready but before it is
                      served, it is still served.  Is this the behavior we want?
                   *)
                   fun loop ready =
                      (case ready of
                          nil =>
                             start ()
                        | sd :: rest =>
                             (* I wish we didn't need a linear search, but equality is the only test we have. *)
                             (case List.find (fn (sd', _) => sameDesc (sd, sd')) (!callbacks) of
                                 NONE =>
                                    raise (Fail "socks and callbacks must match")
                               | SOME (_, f) =>
                                    (case dispatch f of
                                        SHUTDOWN => ()
                                      | YIELD => loop rest)))
                in
                   loop ready
                end)


      fun setTimeout t f =
         (
         timeout := Platform.adjustSelectTimeout t;
         timecall := f
         )

      fun insertSock sock f =
         let
            val sd = S.sockDesc sock

            val callbacks' =
               (sd, f) :: List.filter (fn (sd', _) => not (sameDesc (sd, sd'))) (!callbacks)
         in
            callbacks := callbacks';
            socks := map #1 callbacks'
         end

      fun deleteSock sock =
         let
            val sd = S.sockDesc sock

            val callbacks' =
               List.filter (fn (sd', _) => not (sameDesc (sd, sd'))) (!callbacks)
         in
            callbacks := callbacks';
            socks := map #1 callbacks'
         end

   end
