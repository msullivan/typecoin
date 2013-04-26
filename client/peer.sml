
structure Peer :> PEER =
   struct

      (* Constants *)
      val minPeers = 40
      val maxPeers = 1000
      val tableSize = 2003  (* roughly twice maxPeers *)

      (* Execute maintenance this often. *)
      val maintenanceInterval = Time.fromSeconds (10 * 60)  (* 10 minutes *)

      (* Throw out anything this old. *)
      val tooOld = Time.fromSeconds (24 * 60 * 60)          (* 24 hours (Satoshi uses 14 days!) *)

      (* Don't relay anything this old. *)
      val tooOldToRelay = Time.fromSeconds (3 * 60 * 60)    (* 3 hours *)

      (* Dock the timestamp of incoming relayed peers this much. *)
      val relayedPenalty = Time.fromSeconds (2 * 60 * 60)   (* 2 hours *)

      (* Give dns-acquired peers with a timestamp this long before now.
         This must be greater than tooOldToRelay (so dns-acquired peers are
         not relayed), but less than tooOld (so we can still use them).
      *)
      val dnsPenalty = Time.fromSeconds (4 * 60 * 60)       (* 4 hours *)

      (* Use dns at most this often. *)
      val dnsInterval = Time.fromSeconds (15 * 60)          (* 15 minutes *)


      structure H = HashTable (structure Key = Address.Hashable)
      structure DT = SplayRDict (structure Key = TimeOrdered)
      structure DA = ListPreDict (structure Key = Address.Ordered)
      structure Q = IDeque

      type peer =
         {
         addr : Address.addr,
         timer : Time.time ref,
         noder : Q.idequeNode ref
         }

      fun address ({addr, ...}:peer) = addr

      fun time ({timer, ...}:peer) = !timer


      val theTable : peer H.table = H.table tableSize
      val theOrder : peer DA.dict DT.dict ref = ref DT.empty
      val theQueue : peer Q.ideque = Q.ideque ()
      val numPeers = ref 0
      val lastDns = ref Time.zeroTime

      (* We do not require that the time or peer are already in the dictionary. *)
      fun removeOrder time ({addr, ...}:peer) =
         theOrder :=
         (#3 (DT.operate' (!theOrder) time
                 (fn () => NONE)
                 (fn d =>
                     let
                        val d' = DA.remove d addr
                     in
                        if DA.isEmpty d' then
                           NONE
                        else
                           SOME d'
                     end)))
      
      fun insertOrder time (peer as {addr, ...}:peer) =
         theOrder :=
         (#3 (DT.operate (!theOrder) time
                 (fn () => DA.singleton addr peer)
                 (fn d => DA.insert d addr peer)))


      fun delete (peer as {addr, timer, noder, ...}:peer) =
         if H.member theTable addr then
            (
            H.remove theTable addr;
            removeOrder (!timer) peer;
            (Q.delete (!noder) handle Q.Orphan => ());
            numPeers := !numPeers - 1
            )
         else
            ()


      fun update (peer as {timer, ...}:peer) newtime =
         (* update the time if the new time is more recent *)
         let
            val oldtime = !timer
         in
            if Time.<= (newtime, oldtime) then
               ()
            else
               (
               removeOrder oldtime peer;
               insertOrder newtime peer;
               timer := newtime
               )
         end


      fun new addr newtime =
         if !numPeers >= maxPeers then
            ()
         else
            let
               val (peer as {noder, ...}:peer, old) =
                  H.lookupOrInsert' theTable addr
                  (fn () => {addr=addr, timer=ref Time.zeroTime, noder=ref Q.dummy})
            in
               update peer newtime;

               if old then
                  ()
               else
                  (
                  noder := Q.insertBackNode theQueue peer;
                  numPeers := !numPeers + 1
                  )
            end


      fun next () =
         let
            val peer as {timer, noder, ...}:peer = Q.removeFront theQueue
         in
            if Time.<= (!timer, Time.- (Time.now (), tooOld)) then
               (
               delete peer;
               next ()
               )
            else
               SOME peer
         end handle Q.Empty => NONE


      fun enqueue (peer as {noder, ...}:peer) =
         if Q.orphan (!noder) then
            noder := Q.insertBackNode theQueue peer
         else
            (* already in the queue, ignore *)
            ()


      fun wantPeers () =
         maxPeers - !numPeers 


      fun relayable () =
         let
            val cutoff = Time.- (Time.now (), tooOldToRelay)
         in
            DT.foldr (fn (_, d, l) =>
                         DA.foldr (fn (_, peer as {timer, ...}:peer, l) =>
                                      if Time.>= (!timer, cutoff) then
                                         peer :: l
                                      else
                                         l) l d) [] (!theOrder)
         end
            

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
            val () = Log.log (fn () => "Peer maintenance\n")
            val (old, recent) = DT.partitiongt (!theOrder) (Time.- (Time.now (), tooOld))
         in
            theOrder := recent;
            
            DT.app (fn (_, d) =>
                       DA.app (fn (_, {addr, noder, ...}:peer) =>
                                  (
                                  H.remove theTable addr;
                                  (Q.delete (!noder) handle Q.Orphan => ());
                                  numPeers := !numPeers - 1
                                  )) d) old;

            (* If we need peers, and we haven't used dns too recently, then do so. *)
            if !numPeers < minPeers andalso Time.>= (Time.now (), Time.+ (!lastDns, dnsInterval)) then
               (
               lastDns := Time.now ();
               reseed (Time.- (Time.now (), dnsPenalty)) (map Network.dns Chain.seeds) []
               )
            else
               ()
         end


      fun initialize () =
         (
         H.reset theTable;
         theOrder := DT.empty;
         Q.reset theQueue;
         numPeers := 0;
         lastDns := Time.zeroTime;

         maintenance ();
         Scheduler.repeating maintenanceInterval maintenance;
         ()
         )

   end
