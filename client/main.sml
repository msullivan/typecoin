
structure Main =
   struct

      fun main' () =
         (
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
         Blockchain.initialize ();

         (* start the scheduler and immediately call main' *)
         Scheduler.start main';
         (* done, cleaning up *)

         print (Int.toString (Peer.wantPeers ()))
         )

   end
