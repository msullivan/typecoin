
structure Peer :> PEER =
   struct

      (* Constants *)
      val minPeers = 40
      val maxPeers = 1000
      val tableSize = 2003  (* roughly twice maxPeers *)

      (* Execute maintenance this often. *)
      val maintenanceInterval = Time.fromSeconds (10 * 60)  (* 10 minutes *)

      (* Throw out anything this old. *)
      val tooOld = Time.fromSeconds (14 * 24 * 60 * 60)     (* 14 days *)

      (* Don't relay anything this old. *)
      val tooOldToRelay = Time.fromSeconds (3 * 60 * 60)    (* 3 hours *)

      (* Dock the timestamp of incoming relayed peers this much. *)
      val relayedPenalty = Time.fromSeconds (2 * 60 * 60)   (* 2 hours *)

      (* Give dns-acquired peers a timestamp this long before now.
         This must be greater than tooOldToRelay (so dns-acquired peers are
         not relayed), but less than tooOld (so we can still use them).
      *)
      val dnsPenalty = Time.fromSeconds (4 * 60 * 60)       (* 4 hours *)

      (* After this many connection failures, pull a peer from the verified queue. *)
      val verifiedQueueThreshold = 4


      structure H = HashTable (structure Key = Address.Hashable)
      structure DT = SplayRDict (structure Key = TimeOrdered)
      structure DA = ListPreDict (structure Key = Address.Ordered)
      structure Q = IDeque

      type peer =
         {
         addr : Address.addr,
         timer : Time.time ref,
         noder : Q.idequeNode ref,
         valid : bool ref
         }

      fun address ({addr, ...}:peer) = addr

      fun time ({timer, ...}:peer) = !timer


      val theTable : peer H.table = H.table tableSize
      val theQueue : peer Q.ideque = Q.ideque ()
      val theVerifiedQueue : peer Q.ideque = Q.ideque ()  (* requeued peers, more likely to be active *)
      val queuedPeers = ref 0
      val relayablePeers : Address.addr list ref = ref []

      fun delete (peer as {addr, timer, noder, valid, ...}:peer) =
         if H.member theTable addr then
            (
            H.remove theTable addr;
            ((Q.delete (!noder); queuedPeers := !queuedPeers - 1)
                handle Q.Orphan => ());
            valid := false
            )
         else
            ()


      fun update (peer as {timer, ...}:peer) newtime =
         (* update the time if the new time is more recent *)
         if Time.< (newtime, !timer) then
            ()
         else
            timer := newtime


      fun new addr newtime =
         if !queuedPeers >= maxPeers then
            ()
         else
            let
               val peer as {noder, ...}:peer =
                  H.lookupOrInsert theTable addr
                  (fn () => {addr=addr, timer=ref Time.zeroTime, noder=ref Q.dummy, valid=ref true})
            in
               update peer newtime;

               if Q.orphan (!noder) then
                  (
                  noder := Q.insertBackNode theQueue peer;
                  queuedPeers := !queuedPeers + 1
                  )
               else
                  ()
            end


      fun next failures =
         let
            val peer as {timer, noder, ...}:peer =
               if failures >= verifiedQueueThreshold then
                  Q.removeFront theVerifiedQueue
                  handle Q.Empty => Q.removeFront theQueue
               else
                  Q.removeFront theQueue
                  handle Q.Empty => Q.removeFront theVerifiedQueue
         in
            queuedPeers := !queuedPeers - 1;

            if Time.<= (!timer, Time.- (Time.now (), tooOld)) then
               (
               delete peer;
               next failures
               )
            else
               SOME peer
         end handle Q.Empty => NONE


      fun enqueue (peer as {noder, valid, ...}:peer) =
         if !valid andalso Q.orphan (!noder) then
            noder := Q.insertBackNode theVerifiedQueue peer
         else
            (* invalid or already in the queue, ignore *)
            ()


      fun wantPeers () =
         maxPeers - !queuedPeers 


      fun relayable () = !relayablePeers
            

      (* stripe the seed list so we don't favor one server over another *)
      fun reseed time llin llout =
         (case llin of
             [] =>
                if List.null llout then
                   ()
                else
                   reseed time llout []
          | [] :: llrest =>
               reseed time llrest llout
          | (inaddr :: l) :: llrest =>
               (
               new (Address.fromInAddr inaddr) time;
               reseed time llrest (l :: llout)
               ))

      fun maintenance () =
         let
            val () = Log.long (fn () => "Peer maintenance");

            val now = Time.now ()
            val purgeCutoff = Time.- (now, tooOld)
            val relayCutoff = Time.- (now, tooOldToRelay)

            val (relayable, todelete) =
               H.fold
                  (fn (addr, peer as {timer, noder, valid, ...}:peer, (relayable, todelete)) =>
                      if Time.< (!timer, purgeCutoff) then
                         (relayable, peer :: todelete)
                      else if Time.< (!timer, relayCutoff) then
                         (relayable, todelete)
                      else
                         (addr :: relayable, todelete))
                  ([], []) theTable
         in
            relayablePeers := relayable;
            app delete todelete;

            (* If we need peers, try dns. *)
            if !queuedPeers < minPeers then
               (
               Log.long (fn () => "DNS seeding");
               reseed (Time.- (Time.now (), dnsPenalty)) (map Network.dns Chain.seeds) []
               )
            else
               ();

            Log.long (fn () => "Peer maintenance complete")
         end


(*
      val seeds =
         ["5.9.2.145","23.23.45.26","24.50.2.175","46.4.24.198","62.87.179.182",
          "62.213.207.209","67.20.120.210","78.47.138.172","80.69.77.225",
          "82.69.209.33","83.137.101.103","84.200.17.182","85.214.146.220",
          "94.23.1.23","94.23.47.168","95.170.83.79","96.241.62.47","108.61.77.74",
          "109.238.35.170","142.4.209.33","152.2.31.233","164.177.157.148",
          "173.230.150.38","173.236.193.117","176.9.218.166","178.63.48.141",
          "192.30.35.174","198.199.70.246","199.26.85.40","212.238.236.21",
          "213.5.71.38"]
*)

      fun initialize () =
         (
         H.reset theTable;
         Q.reset theQueue;
         Q.reset theVerifiedQueue;
         queuedPeers := 0;
         relayablePeers := [];

         maintenance ();
         Scheduler.repeating maintenanceInterval maintenance;
         ()
         )

   end
