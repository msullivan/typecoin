
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


      val rsocks : S.sock_desc list ref = ref []
      val wsocks : S.sock_desc list ref = ref []
      val callbacks : (S.sock_desc * (unit -> unit)) list ref = ref []
      val timeout = ref Time.zeroTime
      val timecall : (unit -> unit) ref = ref (fn () => raise Shutdown)

      fun mainloop () =
         (case S.select {rds=(!rsocks), wrs=(!wsocks), exs=[], timeout=SOME (!timeout)} of
             {rds=[], wrs=[], ...} =>
                (case dispatch (!timecall) of
                    SHUTDOWN => ()
                  | YIELD => mainloop ())
           | {rds=rready, wrs=wready, ...} =>
                let
                   (* Note that if a socket is deleted after it becomes ready but before it is
                      served, it is still served.  Is this the behavior we want?
                   *)
                   fun loop ready =
                      (case ready of
                          nil =>
                             mainloop ()
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
                   loop (wready @ rready)
                end)


      fun start f =
         (
         rsocks := [];
         wsocks := [];
         callbacks := [];
         timeout := Time.zeroTime;
         timecall := (fn () => raise Shutdown);

         (case dispatch f of
             SHUTDOWN => ()
           | YIELD => mainloop ())
         )


      fun setTimeout t f =
         (
         timeout := Platform.adjustSelectTimeout t;
         timecall := f
         )

      fun insertRead sock f =
         let
            val sd = S.sockDesc sock
         in
            callbacks := (sd, f) :: List.filter (fn (sd', _) => not (sameDesc (sd, sd'))) (!callbacks);
            rsocks := sd :: List.filter (fn sd' => not (sameDesc (sd, sd'))) (!rsocks)
         end

      fun insertWrite sock f =
         let
            val sd = S.sockDesc sock
         in
            callbacks := (sd, f) :: List.filter (fn (sd', _) => not (sameDesc (sd, sd'))) (!callbacks);
            wsocks := sd :: List.filter (fn sd' => not (sameDesc (sd, sd'))) (!wsocks)
         end

      fun delete sock =
         let
            val sd = S.sockDesc sock

            val callbacks' =
               List.filter (fn (sd', _) => not (sameDesc (sd, sd'))) (!callbacks)
         in
            callbacks := List.filter (fn (sd', _) => not (sameDesc (sd, sd'))) (!callbacks);
            rsocks := List.filter (fn sd' => not (sameDesc (sd, sd'))) (!rsocks);
            wsocks := List.filter (fn sd' => not (sameDesc (sd, sd'))) (!wsocks)
         end

   end
