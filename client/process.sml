
structure Process :> PROCESS =
   struct

      (* Constants *)

      (* Check sync throughput this often. *)
      val syncTimeout = Time.fromSeconds 30          (* 30 seconds *)    

      (* Want to receive at least this many bytes per syncTimeout. *)
      val syncThroughput = 5 * 1024 * 1024           (* 5 MB *)                   

      (* Accept at most this many orphans from any one connection. *)
      val maxOrphans = 50

      (* Accept at most this many addresses from one peer. *)
      val maxPeersFromSource = 40


      structure B = Bytestring
      structure BS = Bytesubstring
      structure M = Message



      (* To avoid wasting bandwidth by downloading the blockchain from multiple peers at once,
         use one peer exclusively as long as we're happy with the progress we're making.
      *)
      val syncing : Commo.conn option ref = ref NONE
      val syncData = ref 0  (* amount of sync data received during this syncTimeout period *)


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

            val hashes = loop [] 10 (Blockchain.lastBlock ())
         in
            Commo.sendMessage conn
            (M.Getblocks
                (M.mkGetblocks
                    { hashes = hashes, lastDesired = goal }))
         end



      fun monitorSync conn remoteblocks =
         if Blockchain.lastBlock () >= remoteblocks then
            (* Done *)
            (* We never got a chance to ask for peers, to ask now. *)
            if Peer.wantPeers () > 0 then
               Commo.sendMessage conn M.Getaddr
            else
               ()
         else if !syncData >= syncThroughput then
            (* Acceptable progress *)
            (
            Scheduler.once syncTimeout (fn () => monitorSync conn remoteblocks);
            syncData := 0
            )
         else
            (* Unsatisfactory sync throughput.  Try other peers, starting one immediately. *)
            (
            Log.long (fn () => "Unsatisfactory sync throughput (" ^ Int.toString (!syncData) ^ ")");
            Commo.closeConn conn false;
            syncing := NONE;
            Commo.resumePolling ()
            )


      fun processConn conn =
         let
            val remoteblocks = Commo.lastBlock conn
         in
            if not (isSome (!syncing)) andalso remoteblocks > Blockchain.lastBlock () then
               (
               Log.long (fn () => "Syncing");
               sendGetBlocks conn NONE;
               Scheduler.once syncTimeout (fn () => monitorSync conn remoteblocks);
               Commo.suspendPolling ();
               syncing := SOME conn;
               syncData := 0
               )
            else if Peer.wantPeers () > 0 then
               Commo.sendMessage conn M.Getaddr
            else
               ()
         end



      fun processMessage (conn, msg) =
         (case msg of

             M.Addr l =>
                if Peer.wantPeers () <= 0 then
                   ()
                else
                   let
                      val () = Log.short "a{"
                      val l' =
                         Mergesort.sort (fn ((t1, _), (t2, _)) => LargeInt.compare (t2, t1))
                         (List.filter
                          (fn (_, { services, address, port }) =>
                              not (Address.eq (address, Address.null)
                                   orelse
                                   port <> Chain.port
                                   orelse
                                   Word64.andb (services, M.serviceNetwork) = 0w0)) l)
                      
                      fun loop n l =
                         (case l of
                             [] => ()
                           | (timestamp, {address, ...}:M.netaddr) :: rest =>
                                (
                                Peer.new address (Time.- (Time.fromSeconds timestamp, Peer.relayedPenalty));
                                loop (n-1) rest
                                ))
                      val () = Log.short "}"
                   in
                      (* Avoid getting too many addresses (that might be bad) from a single source. *)
                      loop (Int.min (Peer.wantPeers (), maxPeersFromSource)) l'
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
                   Commo.sendMessage conn (M.Getdata invs')
                end


           | M.Block blstr =>
                let
                   val blstr' = BS.string blstr
                   val hash = Block.hashBlockString blstr'
                   val orphanage = Commo.orphanage conn

                   val result = Blockchain.insertBlock orphanage hash blstr'
                in
                   (case !syncing of
                       SOME conn' =>
                          if Commo.eq (conn, conn') then
                             syncData := !syncData + B.size blstr'
                          else ()
                     | NONE =>
                          ());

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
                          (case !syncing of
                              SOME _ =>
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
                                       Commo.resumePolling ();
                                       syncing := NONE
                                       )
                                    else
                                       ()
                                 end
                            | NONE =>
                                 Log.long (fn () => "Block " ^ Int.toString (Blockchain.lastBlock ()))))
                end


           | M.Ping nonce =>
                Commo.sendMessage conn (M.Pong nonce)


           | _ =>
                ())



      fun initialize () =
         (
         syncing := NONE;
         Commo.initialize processConn processMessage
         )

   end
