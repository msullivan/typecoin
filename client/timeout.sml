
structure Timeout :> TIMEOUT =
   struct

      (* constants *)
      val heartbeatInterval = Time.fromSeconds 1

      structure Q = PairingPQueue (TimeOrdered)

      type tid = (unit -> unit) ref

      val theQueue : tid Q.pq ref = ref (Q.empty ())

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


      fun heartbeat () =
         (case Q.findMin (!theQueue) of
             NONE => ()
           | SOME (t, fr) =>
                if Time.>= (Time.now (), t) then
                   (
                   theQueue := #2 (Q.deleteMin (!theQueue));
                   !fr ();
                   heartbeat ()
                   )
                else
                   ())


      fun initialize () =
         (
         theQueue := Q.empty ();
         Scheduler.setTimeout heartbeatInterval heartbeat
         )

   end
