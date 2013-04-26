
structure Main =
   struct

      fun main' () =
         (
         Timeout.initialize ();

         (* Initialize Peer before Process, so that there are peers in the queue for Process. *)
         Peer.initialize ();
         Process.initialize ();

         (* XX Ending the world after 40 seconds. *)
         Timeout.once (Time.fromSeconds 40) Scheduler.shutdown;
         ()
         )

      fun main () =
         let
            (* start the scheduler and immediately call main' *)
            val () = Scheduler.start main'
            (* done, cleaning up *)
         in
            print (Int.toString (Peer.wantPeers ()))
         end

   end
