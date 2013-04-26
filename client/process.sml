
structure Process :> PROCESS =
   struct

      (* Constants *)

      val pollInterval = Time.fromSeconds 30  (* Contact a new peer this often.  XX make higher *)


      structure M = Message


      val log : Message.message list ref = ref []

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
                                Word64.andb (services, Message.serviceNetwork) = 0w0
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

           | M.Ping nonce =>
                (
                Commo.sendMessage conn (M.Pong nonce);
                ()
                )

           | _ =>
                ())
         )


      fun pollNewPeer () =
         (case Peer.next () of
             NONE => ()
           | SOME peer =>
                (
                Log.log (fn () => "Adding peer\n");
                Commo.openConn peer
                   (fn conn =>
                       (
                       Log.log (fn () => "Handshake successful\n");

                       if Peer.wantPeers () > 0 then
                          (Commo.sendMessage conn Message.Getaddr; ())
                       else
                          ();

                       ()
                       ))
                ))


      fun initialize () =
         (
         Commo.initialize processMessage;
         (* start with five peers *)
         pollNewPeer ();
         pollNewPeer ();
         pollNewPeer ();
         pollNewPeer ();
         pollNewPeer ();
         Timeout.repeating pollInterval pollNewPeer;
         ()
         )

   end
