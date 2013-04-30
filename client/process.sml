
structure Process :> PROCESS =
   struct

      (* Constants *)

      (* Contact a new peer this often. *)
      val pollInterval = Time.fromSeconds (2 * 60)   (* 2 minutes *)

      (* Check sync throughput this often. *)
      val syncTimeout = Time.fromSeconds 30          (* 30 seconds *)    

      (* Want to receive at least this many blocks per syncTimeout. *)
      val syncThroughput = 500                     

      (* Accept at most this many orphans from any one connection. *)
      val maxOrphans = 50


      structure B = Bytestring
      structure BS = Bytesubstring
      structure M = Message



      (* To avoid wasting bandwidth by downloading the blockchain from multiple peers at once,
         use one peer exclusively as long as we're happy with the progress we're making.
      *)
      val syncing = ref false


      fun sendGetBlocks conn goal =
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
         in
            Commo.sendMessage conn
            (M.Getblocks
                (M.mkGetblocks
                    { hashes = loop [] 10 (Blockchain.lastBlock ()),
                      lastDesired = goal }))
         end



      fun monitorSync remoteblocks oldblocks =
         let
            val blocks = Blockchain.lastBlock ()
         in
            if blocks >= remoteblocks then
               (* Done *)
               ()
            else if blocks - oldblocks >= syncThroughput then
               (* Acceptable progress *)
               (
               Scheduler.once syncTimeout (fn () => monitorSync remoteblocks blocks);
               ()
               )
            else
               (* Unsatisfactory sync throughput.  Try other peers, starting one immediately. *)
               (
               Log.long (fn () => "Unsatisfactory sync throughput (" ^ Int.toString (blocks-oldblocks) ^ ")");
               Scheduler.onceAbs Time.zeroTime pollNewPeer;
               syncing := false
               )
         end


      and pollNewPeer () =
         if !syncing then
            (* Don't poll new peers while syncing. *)
            ()
         else
            Commo.openConns
            (fn conn =>
                let
                   val remoteblocks = Commo.lastBlock conn
                   val lastblock = Blockchain.lastBlock ()
                in
                   Log.long (fn () => "Handshake successful");
                
                   if not (!syncing) andalso remoteblocks > lastblock then
                      (
                      Log.long (fn () => "Syncing");
                      sendGetBlocks conn NONE;
                      Scheduler.once syncTimeout (fn () => monitorSync remoteblocks lastblock);
                      syncing := true
                      )
                   else
                      ();

                   if Peer.wantPeers () > 0 then
                      (
                      Commo.sendMessage conn M.Getaddr;
                      ()
                      )
                   else
                      ();

                   ()
                end)



      fun processMessage (conn, msg) =
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


           | M.Inv [inv as (M.BLOCK, hash)] =>
                (* In the weirdo blockchain download protocol, a single block inventory message can have the
                   special purpose of indicating the final block.  If we've already identified it as such,
                   request another sheaf of ancestors without bothering to download the final block again.
                *)
                if Blockchain.orphanageMember (Commo.orphanage conn) hash then
                   sendGetBlocks conn (SOME hash)
                else if Blockchain.member hash then
                   ()
                else
                   Commo.sendMessage conn (M.Getdata [inv])
                

           | M.Inv invs =>
                let
                   val invs' =
                      List.filter
                         (fn (M.BLOCK, hash) => not (Blockchain.member hash)
                           | _ => false)
                         invs
                in
                   Commo.sendMessage conn (M.Getdata invs');
                   ()
                end


           | M.Block blstr =>
                let
                   val blstr' = BS.string blstr
                   val hash = Block.hashBlockString blstr'
                   val orphanage = Commo.orphanage conn
                   val oldlastblock = Blockchain.lastBlock ()

                   val result = Blockchain.insertBlock orphanage hash blstr'
                in
                   (case result of
                       Blockchain.ORPHAN =>
                          if Blockchain.orphanageSize orphanage <= maxOrphans then
                             (* request the ancestors of the orphan *)
                             sendGetBlocks conn (SOME hash)
                          else
                             (* too many orphans from this connection, drop it *)
                             Commo.closeConn conn false
                     | Blockchain.NOEXTEND => ()
                     | Blockchain.EXTEND =>
                          (* Log the block, perhaps unless syncing. *)
                          if !syncing then
                             let
                                val blocks = Blockchain.lastBlock ()
                             in
                                if blocks mod 100 = 0 then
                                   Log.long (fn () => "Block " ^ Int.toString (Blockchain.lastBlock ()))
                                else
                                   ();

                                if blocks >= Commo.lastBlock conn then
                                   (
                                   Log.long (fn () => "Sync complete at block " ^ Int.toString blocks);
                                   Scheduler.onceAbs Time.zeroTime pollNewPeer;
                                   syncing := false
                                   )
                                else
                                   ()
                             end
                          else
                             Log.long (fn () => "Block " ^ Int.toString (Blockchain.lastBlock ())))
                end


           | M.Ping nonce =>
                (
                Commo.sendMessage conn (M.Pong nonce);
                ()
                )


           | _ =>
                ())



      fun initialize () =
         (
         syncing := false;
         Commo.initialize processMessage;
         pollNewPeer ();
         Scheduler.repeating pollInterval pollNewPeer;
         ()
         )

   end
