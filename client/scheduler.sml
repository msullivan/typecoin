
structure Scheduler :> SCHEDULER =
   struct

      (* constants *)
      val heartbeatInterval = Time.fromSeconds 1


      structure S = Socket
      structure Q = PairingPQueue (TimeOrdered)
      val sameDesc = Socket.sameDesc


      type tid = (unit -> unit) ref


      datatype directive = EXIT | SHUTDOWN
      exception Exit
      exception Shutdown
      fun exit () = raise Exit
      fun shutdown () = raise Shutdown


      fun dispatch f =
         (f (); EXIT)
         handle Exit => EXIT
              | Shutdown => SHUTDOWN
         (* Should we catch exceptions here? *)



      exception SchedulerException of exn


      val rsocks : S.sock_desc list ref = ref []
      val wsocks : S.sock_desc list ref = ref []
      val callbacks : (S.sock_desc * (unit -> unit)) list ref = ref []
      val theQueue : tid Q.pqueue ref = ref (Q.empty ())
      val queueSize = ref 0


      fun timeloop () =
         (case Q.findMin (!theQueue) of
             NONE =>
                wait ()
           | SOME (t, fr) =>
                if Time.>= (Time.now (), t) then
                   (
                   theQueue := #2 (Q.deleteMin (!theQueue));
                   queueSize := !queueSize - 1;
                   (case dispatch (!fr) of
                       SHUTDOWN => ()
                     | EXIT => timeloop ())
                   )
                else
                   wait ())
         
      and wait () =
         let
            val {rds=rready, wrs=wready, ...} =
               S.select {rds=(!rsocks), wrs=(!wsocks), exs=[], timeout=SOME heartbeatInterval}
               handle exn => raise (SchedulerException exn)
         in
            sockloop (wready @ rready)
         end

      and sockloop ready =
         (* Note that if a socket is deleted after it becomes ready but before it is
            served, it is still served.  Is this the behavior we want?
         *)
         (case ready of
             nil =>
                let in
                   Seed.addTimeEntropy ();
                   timeloop ()
                end
           | sd :: rest =>
                (* I wish we didn't need a linear search, but equality is the only test we have. *)
                (case List.find (fn (sd', _) => sameDesc (sd, sd')) (!callbacks) of
                    NONE =>
                       raise (Fail "socks and callbacks must match")
                  | SOME (_, f) =>
                       (case dispatch f of
                           SHUTDOWN => ()
                         | EXIT => sockloop rest)))


      fun start f =
         (
         rsocks := [];
         wsocks := [];
         callbacks := [];
         theQueue := Q.empty ();
         queueSize := 0;

         (case dispatch f of
             SHUTDOWN => ()
           | EXIT => timeloop ())
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
            theQueue := Q.insert (!theQueue) (time, fr);
            queueSize := !queueSize + 1;
            fr
         end

      fun once time f =
         let
            val fr = ref f
         in
            theQueue := Q.insert (!theQueue) (Time.+ (Time.now (), time), fr);
            queueSize := !queueSize + 1;
            fr
         end

      fun repeating time f =
         let
            val fr = ref skip

            fun loop () =
               (
               theQueue := Q.insert (!theQueue) (Time.+ (Time.now (), time), fr);
               queueSize := !queueSize + 1;
               f ()
               )
         in
            fr := loop;
            theQueue := Q.insert (!theQueue) (Time.+ (Time.now (), time), fr);
            queueSize := !queueSize + 1;
            fr
         end

      fun cancel fr = fr := skip

      fun yield time f =
         (
         once time f;
         raise Exit
         )

      fun numberOfTimeouts () = !queueSize

   end
