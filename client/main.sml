
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

         Scheduler.repeating (Time.fromSeconds 30)
         (fn () => Log.long (fn () => "Heartbeat "
                                      ^ Int.toString (Scheduler.numberOfTimeouts ())
                                      ^ " "
                                      ^ Int.toString (Peer.wantPeers ())
                                      ^ " "
                                      ^ Int.toString (Commo.numberOfConnections ())));

         (* Initialize Peer before Process, so that there are peers in the queue for Process. *)
         Peer.initialize ();
         Process.initialize ();

         ()
         )

      fun cleanup () =
         (
         Blockchain.close ();
         Log.cleanup ()
         )

      fun main () =
         (
         Log.initialize ();
         Blockchain.initialize ();

         (* start the scheduler and immediately call main' *)
         Scheduler.start main';
         (* done, cleaning up *)

         cleanup ()
         )

   end
