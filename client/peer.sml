
structure Peer :> PEER =
   struct

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


      val theTable : peer H.table = H.table (2*Constants.maxPeers+1)
      val theQueue : peer Q.ideque = Q.ideque ()
      val theVerifiedQueue : peer Q.ideque = Q.ideque ()  (* requeued peers, more likely to be active *)
      val queuedPeers = ref 0
      val relayablePeers : (Time.time * Address.addr) list ref = ref []

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


      fun new addr =
         let
            val peer as {noder, ...}:peer =
               H.lookupOrInsert theTable addr
               (fn () => {addr=addr, timer=ref Time.zeroTime, noder=ref Q.dummy, valid=ref true})
         in
            if Q.orphan (!noder) then
               (
               noder := Q.insertBackNode theQueue peer;
               queuedPeers := !queuedPeers + 1
               )
            else
               ();

            peer
         end


      fun insertMaybe addr time =
         if !queuedPeers >= Constants.maxPeers then
            ()
         else
            update (new addr) time
            


      fun next failures =
         let
            val peer as {timer, noder, ...}:peer =
               if failures >= Constants.verifiedQueueThreshold then
                  Q.removeFront theVerifiedQueue
                  handle Q.Empty => Q.removeFront theQueue
               else
                  Q.removeFront theQueue
                  handle Q.Empty => Q.removeFront theVerifiedQueue
         in
            queuedPeers := !queuedPeers - 1;

            if Time.<= (!timer, Time.- (Time.now (), Constants.tooOld)) then
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
         Constants.maxPeers - !queuedPeers 


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
               insertMaybe (Address.fromInAddr inaddr) time;
               reseed time llrest (l :: llout)
               ))

      fun maintenance getAddresses =
         let
            val () = Log.long (fn () => "Peer maintenance");

            val now = Time.now ()
            val purgeCutoff = Time.- (now, Constants.tooOld)
            val relayCutoff = Time.- (now, Constants.tooOldToRelay)

            val (relayable, todelete) =
               H.fold
                  (fn (addr, peer as {timer, noder, valid, ...}:peer, (relayable, todelete)) =>
                      if Time.< (!timer, purgeCutoff) then
                         (relayable, peer :: todelete)
                      else if Time.< (!timer, relayCutoff) then
                         (relayable, todelete)
                      else
                         ((!timer, addr) :: relayable, todelete))
                  ([], []) theTable
         in
            relayablePeers := relayable;
            app delete todelete;

            (* If we need peers, try dns. *)
            if !queuedPeers < Constants.minPeers then
               (
               Log.long (fn () => "DNS seeding");
               reseed (Time.- (Time.now (), Constants.dnsPenalty)) (map Network.dns Chain.seeds) []
               )
            else
               ();

            Log.long (fn () => "Peer maintenance complete")
         end


      fun initialize getAddresses =
         (
         H.reset theTable;
         Q.reset theQueue;
         Q.reset theVerifiedQueue;
         queuedPeers := 0;
         relayablePeers := [];

         maintenance getAddresses;
         Scheduler.repeating Constants.maintenanceInterval (fn () => maintenance getAddresses);
         ()
         )

   end
