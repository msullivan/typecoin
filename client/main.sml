
structure Main =
   struct

      fun mainUnderway () =
         (
         (* Initialize Peer first, so that there are peers in the queue for Process. *)
         Peer.initialize ();
         Process.initialize ();

         (* XX Ending the world after 40 seconds. *)
         Timeout.once (Time.fromSeconds 40) Scheduler.shutdown;
         ()
         )

      fun main () =
         let
            val () = Scheduler.cleanup ()
            val () = Timeout.initialize ()
            val _ = Timeout.onceAbs Time.zeroTime mainUnderway

            (* start the scheduler and immediately (in one heartbeat) call mainUnderway *)
            val () = Scheduler.start ()
            (* done, cleaning up *)
         in
            print (Int.toString (!Peer.numPeers))
         end

   end
