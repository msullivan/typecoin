
structure Process :> PROCESS =
   struct

      structure B = Bytestring
      structure BS = Bytesubstring
      structure M = Message


      structure HashHashable =
         struct
            type t = Bytestring.string
            val eq = B.eq

            fun hash str =
               (* The front bytes are pretty much random, so just use those. *)
               ConvertWord.wordLgToWord (PackWord32Big.subVec (str, 0))
         end
      structure T = HashTable (structure Key = HashHashable)

      val dhash = SHA256.hashBytes o SHA256.hashBytes



      (* The Bitcoin block download protocol is a bit strange.  (The Bitcoinj client calls it
         "very implicit and not well thought out".)

         1. You send a Getblocks message with a list of block hashes you already have (the first
            ten or so sequentially, and then doubling the increment each entry thereafter), and
            with an empty desired block, since you don't know what the last block's hash is.

            The history of hashes you already have is so the remote can determine the last block
            you have that is on the main fork.  

         2. The remote peer responds with an Inv message containing 500 block hashes.

         3. You send a Getdata message requesting those 500 blocks, and the remote sends you the
            blocks.

         4. (Here's the strange part.)  When you request the last of the 500 blocks, the remote
            sends you an Inv message with the most recent block.  You send a Getdata message
            requesting that block.  The remote sends you that block, which you discover is
            an orphan block.  You store it off the blockchain.

         5. You send a Getblocks message with the remote's most recent block as the desired
            block.  (You cannot just repeat step 1 and send the Getblocks message without the
            desired block again.  For reasons I don't understand -- and in apparent contradiction
            of the protocol specification given on the Bitcoin wiki -- the remote won't respond
            to it again.  Except sometimes.)

         6. The remote peer responds with an Inv message with 500 more block hashes, you request
            them and the remote sends them to you.

         7. When you request the last of the 500 blocks, the remote again sends you an Inv
            message with the most recent block (as in step 4).  You recognize it as a stored
            orphan, so you don't request it again.  You skip to requestng 500 more block hashes
            with that orphan again as the desired block (as in step 5).

         8. Repeat steps 5-7 until the predecessor of the stored orphan arrives.  When you
            detect that the predecessor of a known orphan has arrived, you finally link
            in that orphan and terminate the sync process.
      *)

      fun getblocks goal =
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
            M.Getblocks (M.mkGetblocks { hashes = hashes, lastDesired = goal })
         end


      (* To avoid wasting bandwidth by downloading the blockchain from multiple peers at once,
         use one peer exclusively as long as we're happy with the progress we're making.
         Also, we suspect verification while syncing, since there's a good chance of getting
         more than chainTrustConfirmations blocks.
      *)
      val syncing : Commo.conn option ref = ref NONE
      val syncData = ref 0  (* amount of sync data received during this syncTimeout period *)

      fun syncingWith conn =
         (case !syncing of
             NONE => false
           | SOME conn' => Commo.eq (conn, conn'))


      fun monitorSync conn remoteblocks =
         if Blockchain.lastBlock () >= remoteblocks orelse not (isSome (!syncing)) then
            (* Done or aborted. *)
            ()
         else if !syncData >= Constants.syncThroughput then
            (* Acceptable progress *)
            (
            Scheduler.once Constants.throughputInterval (fn () => monitorSync conn remoteblocks);
            Log.long (fn () => "Sync throughput: " ^ (Int.toString (!syncData div 1024)));
            syncData := 0
            )
         else
            (* Unsatisfactory sync throughput.  Try other peers, starting one immediately. *)
            (
            Log.long (fn () => "Unsatisfactory sync throughput: " ^ Int.toString (!syncData div 1024));
            Blockchain.resumeVerification ();
            Commo.closeConn conn false;
            Commo.resumePolling ();
            syncing := NONE
            )


      (* complete=true if sync complete, complete=false if sync aborted. *)
      fun doneSync complete =
         let in
            Blockchain.resumeVerification ();
   
            Log.long (fn () => if complete then
                                  "Sync complete at block " ^ Int.toString (Blockchain.lastBlock ())
                               else
                                  "Sync aborted at block " ^ Int.toString (Blockchain.lastBlock ()));

            Blockchain.writeIndex ();
            Commo.resumePolling ();

            (* We never got a chance to ask for peers, so ask now. *)
            if Peer.wantPeers () > 0 then
               Commo.sendMessage (valOf (!syncing)) M.Getaddr
            else
               ();

            syncing := NONE
         end


      fun processConn conn =
         let
            val remoteblocks = Commo.lastBlock conn
            val lastblock = Blockchain.lastBlock ()
         in
            if not (isSome (!syncing)) andalso remoteblocks > lastblock then
               (
               Log.long (fn () => "Block " ^ Int.toString remoteblocks ^ " available; syncing");
               Commo.sendMessage conn (getblocks NONE);
               Scheduler.once Constants.throughputInterval (fn () => monitorSync conn remoteblocks);
               Commo.suspendPolling ();
               Blockchain.suspendVerification ();
               syncing := SOME conn;
               syncData := 0
               )
            else if Peer.wantPeers () > 0 then
               Commo.sendMessage conn M.Getaddr
            else
               ()
         end



      val txpool : Bytestring.string T.table = T.table Constants.poolSize
      val relayList : Message.inv list ref = ref []



      fun processMessage (conn, msg) =
         (case msg of

             M.Addr l =>
                (
                Log.short "a";
                if Peer.wantPeers () <= 0 then
                   ()
                else
                   let
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
                                Peer.new address (Time.- (Time.fromSeconds timestamp, Constants.relayedPenalty));
                                loop (n-1) rest
                                ))
                   in
                      (* Avoid getting too many addresses (which might be turn out to be bad) from a single source. *)
                      loop (Int.min (Peer.wantPeers (), Constants.maxPeersFromSource)) l'
                   end
                )


           | M.Inv invs =>
                let
                   val () = Log.short "i"

                   val orphanage = Commo.orphanage conn

                   val (invs', msgs) =
                      foldr
                      (fn (inv as (objtp, hash), (invs', msgs)) =>
                          (case objtp of
                              M.BLOCK =>
                                 if Blockchain.member hash then
                                    (invs', msgs)
                                 else if Blockchain.orphanageMember orphanage hash then
                                    (* We've already identified this block as an orphan, so go directly
                                       to downloading its ancestors without requesting the block again.
                                    *)
                                    (invs', getblocks (SOME hash) :: msgs)
                                 else
                                    (inv :: invs', msgs)
                            | M.TX =>
                                 if T.member txpool hash then
                                    (invs', msgs)
                                 else
                                    (inv :: invs', msgs)
                            | _ =>
                                 (invs', msgs)))
                       ([], [])
                       invs
                in
                   app (Commo.sendMessage conn) (M.Getdata invs' :: msgs)
                end


           | M.Getdata invs =>
                let
                   val () = Log.short "G"

                   val notfound =
                      List.foldl
                      (fn (inv as (objtp, hash), notfound) =>
                          (case objtp of
                              M.ERR => notfound
                            | M.TX =>
                                 (case T.find txpool hash of
                                     NONE =>
                                        inv :: notfound
                                   | SOME txstr =>
                                        let in
                                           Commo.sendMessage conn (M.Tx txstr);
                                           notfound
                                        end)
                            | M.BLOCK =>
                                 (let in
                                     Commo.sendMessage conn (M.Block (Blockchain.blockData hash));
                                     notfound
                                  end
                                  handle Blockchain.Absent => (inv :: notfound))))
                      []
                      invs
                in
                   (case notfound of
                       [] => ()
                     | _ =>
                          Commo.sendMessage conn (M.Notfound (rev notfound)))
                end


           | M.Notfound _ =>
                Log.short "n"



           | M.Getblocks _ =>
                Log.short "B"


           | M.Tx txstr =>
                let
                   val () = Log.short "t"

                   val hash = dhash txstr
                in
                   (if T.member txpool hash then
                       ()
                    else 
                       let
                          val tx = Reader.readfull Transaction.reader (BS.full txstr)
                       in
                          if Verify.verifyTx tx then
                             let in
                                T.insert txpool hash txstr;
                                relayList := (M.TX, hash) :: !relayList
                             end
                          else
                             ()
                       end)
                   handle Reader.SyntaxError => ()
                end


           | M.Block blstr =>
                let
                   val () = Log.short "b"
                   val eblock = EBlock.fromBytes blstr
                in
                   if Verify.verifyBlockGross eblock then
                      let
                         val hash = EBlock.hash eblock
                         val orphanage = Commo.orphanage conn

                         val result = Blockchain.insertBlock orphanage eblock
                      in
                         if syncingWith conn then
                            syncData := !syncData + B.size blstr
                         else
                            ();

                         (case result of
                             Blockchain.ORPHAN => ()
                           | Blockchain.REPEAT => ()
                           | _ =>
                                (case !syncing of
                                    SOME _ => ()
                                  | NONE =>
                                       (* Ideally, we would also relay any orphans that can now be connected.
                                          But it would require some more plumbing to get access to those and
                                          it doesn't seem worth the trouble.  Usually we'll only receive orphans
                                          while syncing anyway.
                                       *)
                                       relayList := (M.BLOCK, EBlock.hash eblock) :: !relayList));

                         (case result of
                             Blockchain.ORPHAN =>
                                if Blockchain.orphanageSize orphanage <= Constants.maxOrphans then
                                   (* request the ancestors of the orphan *)
                                   Commo.sendMessage conn (getblocks (SOME hash))
                                else
                                   (* Too many orphans from this connection, drop it.
      
                                      This can result in an unnecessary disconnection if more than maxOrphans
                                      blocks are mined during a sync, which can happen during the initial
                                      sync.  This isn't terrible, since there are more peers to be contacted
                                      (indeed, maybe it's best to spread the load anyawy).  But we could fix
                                      this by limiting orphan chains, rather than individual orphans.
                                   *)
                                   Commo.closeConn conn false
                           | Blockchain.REPEAT => ()
                           | Blockchain.NOEXTEND => ()
                           | Blockchain.EXTEND =>
                                (case !syncing of
                                    SOME _ =>
                                       let
                                          val blocks = Blockchain.lastBlock ()
                                       in
                                          if blocks mod 100 = 0 then
                                             Log.long (fn () => "Block " ^ Int.toString blocks)
                                          else
                                             ();
          
                                          if blocks >= Commo.lastBlock conn then
                                             (* We probably have all the blocks from conn at this point.  It's posssible
                                                we don't (if there's currently a fork), but even if so, it's no big deal
                                                to shut down sync early.  The last few blocks will still arrive, they
                                                just won't arrive in sync mode.
                                             *)
                                             doneSync true
                                          else
                                             ()
                                       end
                                  | NONE =>
                                       Log.long (fn () => "Block " ^ Int.toString (Blockchain.lastBlock ()))))
                      end
                   else
                      (* This block is bogus.  Don't talk to this peer any more. *)
                      let in
                         Log.long (fn () => "Invalid block detected at " ^ B.toStringHex (B.rev (EBlock.hash eblock)));

                         if syncingWith conn then
                            doneSync false
                         else
                            ();

                         Commo.closeConn conn true
                      end
                end


           | M.Getaddr =>
                let
                   val () = Log.short "A"

                   val l =
                      map
                      (fn (timestamp, addr) =>
                          (Time.toSeconds timestamp,
                           { services = Message.serviceNetwork,
                             address = addr,
                             port = Chain.port }))
                      (Peer.relayable ())
                in
                   Commo.sendMessage conn (M.Addr l)
                end


           | M.Ping nonce =>
                (
                Log.short "P";
                Commo.sendMessage conn (M.Pong nonce)
                )


           | M.Pong _ =>
                Log.short "p"


           | M.Alert _ =>
                Log.short "!"


           | _ =>
                Log.short "?")



      fun relay () =
         (case !relayList of
             [] => ()
           | l as _ :: _ =>
                let in
                   Log.long (fn () => "Relaying "^ Int.toString (length l) ^" items");
                   Commo.broadcastMessage (M.Inv (rev l));
                   relayList := [];

                   (* Wait a while and then remove these from the pool. *)
                   Scheduler.yield Constants.poolRetentionTime
                   (fn () =>
                       List.app
                       (fn (M.TX, hash) => T.remove txpool hash
                         | _ => ())
                       l)
                end)



      fun initialize () =
         (
         syncing := NONE;
         T.reset txpool Constants.poolSize;
         relayList := [];

         (* Initialize Peer before Commo, so that there are peers in the queue for Commo. *)
         Peer.initialize ();
         Commo.initialize processConn processMessage;

         Scheduler.repeating Constants.relayInterval relay;
         ()
         )

   end
