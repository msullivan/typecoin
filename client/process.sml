
structure Process :> PROCESS =
   struct

      (* Constants *)

      val pollInterval = Time.fromSeconds 30  (* Contact a new peer this often.  XX make higher *)

      val syncTimeout = Time.fromSeconds 30   (* Check sync throughput this often. *)
      val syncThroughput = 500                (* Want to receive at least this many blocks per syncTimeout. *)


      structure B = Bytestring
      structure BS = Bytesubstring
      structure M = Message


      (* XX *)
      val log : Message.message list ref = ref []


      datatype sync_state =
         NoSync
       | SyncGetblocks of Commo.conn      (* Block hashes requested *)
       | SyncGetdata of Commo.conn * int  (* Blocks requested; Getdata (_, n) => still awaiting n *)
      
      val syncstate = ref NoSync
      val synctid = ref Scheduler.dummy

      fun syncing () =
         (case !syncstate of
             NoSync => false
           | _ => true)



      fun processMessage (conn, msg) =
         (
         log := msg :: !log;
         (case msg of

             M.Addr l =>
                let
                   fun loop l =
                      (case l of
                          [] => ()
                        | (timestamp, { services, address, port }) :: rest =>
                             if Peer.wantPeers () <= 0 then
                                ()
                             else if
                                Address.eq (address, Address.null)
                                orelse
                                port <> Chain.port
                                orelse
                                Word64.andb (services, M.serviceNetwork) = 0w0
                             then
                                (* Ignore peers with null addresses, or at a non-standard port,
                                   or that don't offer the network service. *)
                                loop rest
                             else
                                (
                                Peer.new address (Time.- (Time.fromSeconds timestamp, Peer.relayedPenalty));
                                loop rest
                                ))
                in
                   loop l
                end

           | M.Inv invs =>
                if
                   (case !syncstate of
                       SyncGetblocks conn' => Commo.eq (conn, conn')
                     | _ => false)
                then
                   let
                      fun loop blcount lastbl acc l =
                         (case l of
                             nil =>
                                (blcount, lastbl, rev acc)
                           | (inv as (objtp, hash)) :: rest =>
                                (case objtp of
                                    M.BLOCK =>
                                       if Blockchain.member hash then
                                          loop blcount hash acc rest
                                       else
                                          loop (blcount+1) hash (inv :: acc) rest
                                  | M.TX =>
                                       (* ignore for now *)
                                       loop blcount lastbl acc rest
                                  | M.ERR =>
                                       loop blcount lastbl acc rest))
   
                      val (blcount, lastbl, invs') = loop 0 B.null [] invs
                   in
                      (case invs' of
                          [] => ()
                        | _ =>
                             (Commo.sendMessage conn (M.Getdata invs'); ()));

                      if blcount > 0 then
                         (* Received block hashes we don't have.  We just requested them; await their arrival. *)
                         (
                         Log.log (fn () => "Sync received " ^ Int.toString blcount ^ " new hashes\n");
                         syncstate := SyncGetdata (conn, blcount)
                         )
                      else if B.eq (lastbl, B.null) then
                         (* Received no block hashes at all. *)
                         ()
                      else
                         (* Received block hashes, but none we don't have yet.  Keep trying. *)
                         (
                         Log.log (fn () => "Sync received no new hashes\n");
                         Commo.sendMessage conn (M.Getblocks (M.mkGetblocks { hashes=[lastbl], lastDesired=NONE }));
                         ()
                         )
                   end
                else
                   (
                   Commo.sendMessage conn
                      (M.Getdata (List.filter
                                     (fn (M.BLOCK, hash) => not (Blockchain.member hash)
                                       | _ => false)
                                     invs));
                   ()
                   )

           | M.Block blstr =>
                let
                   val blstr' = BS.string blstr
                   val hash = Block.hashBlockString blstr'
                in
                   Blockchain.insertBlock hash blstr';
   
                   (case !syncstate of
                       SyncGetdata (conn', n) =>
                          if Commo.eq (conn, conn') then
                             if n > 1 then
                                (* Still awaiting more blocks. *)
                                syncstate := SyncGetdata (conn, n-1)
                             else if Commo.lastBlock conn > Blockchain.lastBlock () then
                                (* That completes this batch, but there's still more to download. *)
                                if
                                   Commo.sendMessage conn
                                   (M.Getblocks (M.mkGetblocks { hashes=[Block.hashBlockString blstr'], lastDesired=NONE }))
                                then
                                   ()
(*
                                   syncstate := SyncGetblocks conn
*)
                                else
                                   (* sendMessage failed *)
                                   (
                                   syncstate := NoSync;
                                   Scheduler.cancel (!synctid);
                                   Log.log (fn () => "Sync aborted\n")
                                   )
                             else
                                (* Done with sync. *)
                                (
                                Scheduler.cancel (!synctid);
                                syncstate := NoSync;
                                Log.log (fn () => "Sync completed\n")
                                )
                          else
                             ()
                     | _ => ())
                end

           | M.Ping nonce =>
                (
                Commo.sendMessage conn (M.Pong nonce);
                ()
                )

           | _ =>
                ())
         )


      fun startSync conn =
         let
            fun loopGeom acc inc num =
               if num < 0 then
                  rev acc
               else
                  loopGeom (Blockchain.hashByNumber num :: acc) (inc*2) (num-inc)

            fun loop acc n num =
               if num < 0 then
                  rev acc
               else if n <= 0 then
                  loopGeom acc 2 num
               else
                  loop (Blockchain.hashByNumber num :: acc) (n-1) (num-1)

            val success =
               Commo.sendMessage conn
               (M.Getblocks
                   (M.mkGetblocks
                       { hashes = loop [] 10 (Blockchain.lastBlock ()),
                         lastDesired = NONE }))

            fun monitor oldlastblock throughputGoal =
               (* We've suspending contacting peers while we download the blockchain, so we want
                  to make sure we're making good progress.
               *)
               let
                  val lastblock = Blockchain.lastBlock ()
               in
                  if lastblock >= Commo.lastBlock conn then
                     (* We're done; someone else sent us the last block we were looking for. *)
                     (
                     Log.log (fn () => "Sync completed inadvertently\n");
                     syncstate := NoSync
                     )
                  else if lastblock - oldlastblock >= throughputGoal then
                     (* Making acceptable progress, keep going. *)
                     synctid :=
                        Scheduler.once syncTimeout (fn () => monitor lastblock syncThroughput)
                  else
                     (
                     Log.log (fn () => "Unsatisfactory sync throughput (" ^ Int.toString (lastblock - oldlastblock) ^ ")\n");
                     syncstate := NoSync
                     )
               end
         in
            if success then
               let
                  val lastblock = Blockchain.lastBlock ()
               in
                  Log.log (fn () => "Sync initiated\n");
                  syncstate := SyncGetblocks conn;
                  synctid :=
                     Scheduler.once syncTimeout (fn () => monitor lastblock 1)  (* set a modest goal for the first timeout *)
               end
            else
               ()
         end


      fun pollNewPeer () =
         if syncing () then
            (* Don't poll new peer while downloading the blockchain. *)
            ()
         else
            (case Peer.next () of
                NONE => ()
              | SOME peer =>
                   (
                   Log.log (fn () => "Adding peer\n");
                   Commo.openConn peer
                      (fn conn =>
                          (
                          Log.log (fn () => "Handshake successful\n");
   
                          if not (syncing ()) andalso Commo.lastBlock conn > Blockchain.lastBlock () then
                             startSync conn
                          else if Peer.wantPeers () > 0 then
                             (
                             Commo.sendMessage conn M.Getaddr;
                             ()
                             )
                          else
                             ()
                          ))
                   ))


      fun initialize () =
         (
         syncstate := NoSync;
         Commo.initialize processMessage;
         (* start with five peers *)
         pollNewPeer ();
         pollNewPeer ();
         pollNewPeer ();
         pollNewPeer ();
         pollNewPeer ();
         Scheduler.repeating pollInterval pollNewPeer;
         ()
         )

   end
