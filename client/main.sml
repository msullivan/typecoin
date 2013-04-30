
structure Main =
   struct

      fun midnight () =
         let
            val date = Date.fromTimeLocal (Time.now ())
         in
            Date.toTime
            (Date.date { year=Date.year date, month=Date.month date, day=Date.day date,
                         hour=0, minute=0, second=0, offset=NONE })
         end

      fun main' () =
         (
         Scheduler.onceAbs (Time.+ (midnight (), Time.fromSeconds (60 * 60 * 24)))
         (fn () => (
                   Log.long (fn () => Date.toString (Date.fromTimeLocal (Time.now ())));
                   Scheduler.onceAbs (Time.+ (midnight (), Time.fromSeconds (60 * 60 * 24)));
                   ()
                   ));

         (* Initialize Peer before Process, so that there are peers in the queue for Process. *)
         Peer.initialize ();
         Process.initialize ();

(*
         (* XX Ending the world after 40 seconds. *)
         Scheduler.once (Time.fromSeconds 40) Scheduler.shutdown;
*)
         ()
         )

      fun main () =
         (
         Log.long (fn () => Date.toString (Date.fromTimeLocal (Time.now ())));
         Blockchain.initialize ();

         (* start the scheduler and immediately call main' *)
         Scheduler.start main';
         (* done, cleaning up *)

         ()
         )

   end
