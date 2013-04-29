
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


           | M.Inv [inv as (M.BLOCK, hash)] =>
                (* In the weirdo blockchain download protocol, a single block inventory message can have
                   special purpose of indicating the final block.  If we've already identified it as such,
                   request another sheaf of ancestors without bothering to download the final block again.
                *)
                if Blockchain.knownOrphan hash then
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
                in
                   if Blockchain.insertBlock hash blstr' then
                      (* Orphan, request its ancestors. *)
                      sendGetBlocks conn (SOME hash)
                   else
                      ()
                end

           | M.Ping nonce =>
                (
                Commo.sendMessage conn (M.Pong nonce);
                ()
                )

           | _ =>
                ())
         )



      (* To avoid wasting bandwidth by downloading the blockchain from multiple peers at once,
         use one peer exclusively as long as we're happy with the progress we're making.
      *)
      val syncing = ref false


      fun monitorSync remoteblocks oldblocks =
         let
            val blocks = Blockchain.lastBlock ()
         in
            if blocks >= remoteblocks then
               (* Done *)
               (
               Log.log (fn () => "Sync complete\n");
               syncing := false
               )
            else if blocks - oldblocks >= syncThroughput then
               (* Acceptable progress *)
               (
               Scheduler.once syncTimeout (fn () => monitorSync remoteblocks blocks);
               ()
               )
            else
               (* Unsatisfactory sync throughput.  Try other peers. *)
               (
               Log.log (fn () => "Unsatisfactory sync throughput (" ^ Int.toString (blocks-oldblocks) ^ ")\n");
               syncing := false
               )
         end


      fun pollNewPeer () =
         (case Peer.next () of
             NONE => ()
           | SOME peer =>
                (
                Log.log (fn () => "Adding peer\n");
                Commo.openConn peer
                   (fn conn =>
                       let
                          val remoteblocks = Commo.lastBlock conn
                          val lastblock = Blockchain.lastBlock ()
                       in
                          Log.log (fn () => "Handshake successful\n");

                       
                          if not (!syncing) andalso remoteblocks > lastblock then
                             (
                             Log.log (fn () => "Syncing\n");
                             sendGetBlocks conn NONE;
                             Scheduler.once syncTimeout (fn () => monitorSync remoteblocks lastblock);
                             syncing := true
                             )
                          else if Peer.wantPeers () > 0 then
                             (
                             Commo.sendMessage conn M.Getaddr;
                             ()
                             )
                          else
                             ()
                       end)
                ))


      fun initialize () =
         (
         syncing := false;
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
