
structure Scheduler :> SCHEDULER =
   struct

      (* constants *)
      val heartbeatInterval = Time.fromSeconds 1


      structure S = Socket
      structure Q = PairingPQueue (TimeOrdered)
      val sameDesc = Platform.Socket_sameDesc


      type tid = (unit -> unit) ref


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
      val theQueue : tid Q.pq ref = ref (Q.empty ())


      fun timeloop () =
         (case Q.findMin (!theQueue) of
             NONE =>
                wait ()
           | SOME (t, fr) =>
                if Time.>= (Time.now (), t) then
                   (
                   theQueue := #2 (Q.deleteMin (!theQueue));
                   (case dispatch (!fr) of
                       SHUTDOWN => ()
                     | YIELD => timeloop ())
                   )
                else
                   wait ())
         
      and wait () =
         let
            val {rds=rready, wrs=wready, ...} =
               S.select {rds=(!rsocks), wrs=(!wsocks), exs=[], timeout=SOME heartbeatInterval}
         in
            sockloop (wready @ rready)
         end

      and sockloop ready =
         (* Note that if a socket is deleted after it becomes ready but before it is
            served, it is still served.  Is this the behavior we want?
         *)
         (case ready of
             nil =>
                timeloop ()
           | sd :: rest =>
                (* I wish we didn't need a linear search, but equality is the only test we have. *)
                (case List.find (fn (sd', _) => sameDesc (sd, sd')) (!callbacks) of
                    NONE =>
                       raise (Fail "socks and callbacks must match")
                  | SOME (_, f) =>
                       (case dispatch f of
                           SHUTDOWN => ()
                         | YIELD => sockloop rest)))


      fun start f =
         (
         rsocks := [];
         wsocks := [];
         callbacks := [];
         theQueue := Q.empty ();

         (case dispatch f of
             SHUTDOWN => ()
           | YIELD => timeloop ())
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



      fun skip () = ()

      val dummy : tid = ref skip

      fun onceAbs time f =
         let
            val fr = ref f
         in
            theQueue := Q.insert (time, fr) (!theQueue);
            fr
         end

      fun once time f =
         let
            val fr = ref f
         in
            theQueue := Q.insert (Time.+ (Time.now (), time), fr) (!theQueue);
            fr
         end

      fun repeating time f =
         let
            val fr = ref skip

            fun loop () =
               (
               theQueue := Q.insert (Time.+ (Time.now (), time), fr) (!theQueue);
               f ()
               )
         in
            fr := loop;
            theQueue := Q.insert (Time.+ (Time.now (), time), fr) (!theQueue);
            fr
         end

      fun cancel fr = fr := skip

   end
