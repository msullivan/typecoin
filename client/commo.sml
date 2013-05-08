
structure Commo :> COMMO =
   struct

      structure B = Bytestring
      structure BS = Bytesubstring
      structure M = Message


      (* constants *)

      (* Reject any message with a payload larger than this. *)
      val maximumPayload = 1800000                         (* 1.8 million bytes *)




      (* precomputed values *)
      val netAddrNull = M.mkNetaddr Address.null
      val magic = BS.full (ConvertWord.word32ToBytesL Chain.magic)
      val msgVerack = M.writeMessage M.Verack


      fun recvMessage fk sk =
         let
            fun k str =
               if BS.eq (BS.slice (str, 0, SOME 4), magic) then
                  let
                     val sz = Word32.toInt (ConvertWord.bytesToWord32SL (BS.slice (str, 16, SOME 4)))
                  in
                     if sz > maximumPayload then
                        (fk (); Sink.DONE)
                     else
                        Sink.MORE (sz, fk, (fn str' => sk (M.readMessage (BS.full (BS.concat [str, str'])))))
                  end
               else
                  (* bad magic number *)
                  (fk (); Sink.DONE)
         in
            Sink.MORE (24, fk, k)
         end


      (* Connect to addr, handshake, then return the sock to sk, which must provide a sink.
         If connection or handshaking fails, invoke fk.
      *)
      fun contact addr port fk sk =
         let
            val (sock, already) = Network.connectNB (Address.toInAddr addr, port)

            fun fk' () =
               (
               Log.long (fn () => "Handshake failed with " ^ Address.toString addr);
               fk ()
               )

            fun handshake () =
               let
                  val nonce = ConvertWord.bytesToWord64B (AESFortuna.random 8)
      
                  val msgVersion =
                     M.writeMessage
                        (M.Version
                            (M.mkVersion
                                {
                                self = netAddrNull,
                                remote = M.mkNetaddr addr,
                                nonce = nonce,
                                lastBlock = 0
                                }))
      
                  fun k msg =
                     (case msg of
                         M.Version (ver as {services, ...}) =>
                            if Word64.andb (services, Message.serviceNetwork) = 0w0 then
                               (* Ignore peers that don't offer the network service.  If there were a lot
                                  of them, it might be better to use them for the services they do offer.
                               *)
                               (fk' (); Sink.DONE)
                            else
                               recvMessage fk'
                               (fn M.Verack =>
                                      if Network.sendVec (sock, BS.full msgVerack) then
                                         (
                                         Log.long (fn () => "Handshake successful with " ^ Address.toString addr);
                                         sk (sock, ver)
                                         )
                                      else
                                         (fk' (); Sink.DONE)
                                 | _ =>
                                      (* Did not handshake correctly. *)
                                      (fk' (); Sink.DONE))
                       | _ =>
                            (* Did not handshake correctly. *)
                            (fk' (); Sink.DONE))
      
               in
                  (* Send version, expect version and verack, then send verack. *)
                  if Network.sendVec (sock, BS.full msgVersion) then
                     Sink.register sock (recvMessage fk' k)
                  else
                     fk' ()
               end
         in
            if already then
               (* Does this ever happen? *)
               handshake ()
            else
               let
                  val tid =
                     Scheduler.once Constants.connectTimeout
                     (fn () =>
                         (
                         Log.long (fn () => "Timeout connecting to " ^ Address.toString addr);
                         Scheduler.delete sock;
                         Network.close sock;
                         fk ()
                         ))
               in
                  Scheduler.insertWrite sock
                  (fn () =>
                      (
                      Scheduler.cancel tid;
                      Scheduler.delete sock;
                      handshake ()
                      ))
               end
         end



      type conn = 
         {
         sock : Network.asock,
         peer : Peer.peer,
         lastHeard : Time.time ref,  (* zero indicates forcibly closed *)
         opn : bool ref,             (* still open?  also used for quick equality test *)

         (* Data held for other modules.  If there's too many of these, we should functorize over them. *)
         lastBlock : int,
         orphanage : Blockchain.orphanage
         }


      val theMsgCallback : (conn * Message.message -> unit) ref = ref (fn _ => ())
      val theConnCallback : (conn -> unit) ref = ref (fn _ => ())
      val allConnections : conn list ref = ref []
      val numConnections = ref 0
      val pollingTid = ref Scheduler.dummy


      fun closeConn ({sock, peer, opn, ...}:conn) withPrejudice =
         if !opn then
            (
            Scheduler.delete sock;
            Network.close sock;
            opn := false;
            if withPrejudice then
               (* Closed with prejudice, delete the peer. *)
               Peer.delete peer
            else
               (* Closed without prejudice, re-enqueue it. *)
               Peer.enqueue peer
            )
         else
            (* already closed *)
            ()


      fun closed ({opn, ...}:conn) = not (!opn)


      fun reapIdleConnections () =
         let
            val () = Log.long (fn () => "Reaping")
            val cutoff = Time.- (Time.now (), Constants.idleTimeout)

            val (n, l) =
               foldl (fn (conn as {sock, peer, lastHeard, opn, ...}:conn, (n, l)) =>
                         if Time.>= (!lastHeard, cutoff) then
                            (n+1, conn :: l)
                         else 
                            (
                            Log.long (fn () => "Disconnecting " ^ Address.toString (Peer.address peer));
                            closeConn conn false;
                            (n, l)))
               (0, []) (!allConnections)
         in
            numConnections := n;
            allConnections := l
         end


      fun processMessage (conn as ({peer, lastHeard, ...}:conn)) msg =
         let
            val time = Time.now ()
         in
            lastHeard := time;

            (* If we haven't updated the peer since the threshold time, do so. *)
            if Time.>= (time, Time.+ (Peer.time peer, Constants.peerUpdateThreshold)) then
               Peer.update peer time
            else
               ();
   
            !theMsgCallback (conn, msg)
         end


      fun openConn peer fk sk =
         contact (Peer.address peer) Chain.port
         (fn () =>
             (* Couldn't contact this peer.  Delete it from the peer set. *)
             (
             Peer.delete peer;
             fk ()
             ))
         (fn (sock, {lastBlock, ...}:M.version) =>
             let
                val lastHeard = ref (Time.now ())
                val conn = { sock=sock, peer=peer, lastHeard=lastHeard, opn=ref true,
                             lastBlock=lastBlock, orphanage=Blockchain.newOrphanage () }

                fun loop () =
                   recvMessage
                   (fn () => closeConn conn true)
                   (fn msg =>
                       (
                       processMessage conn msg;
                       loop ()
                       ))
             in
                Peer.update peer (Time.now ());
                numConnections := !numConnections + 1;
                allConnections := conn :: !allConnections;
                sk conn;
                loop ()
             end)



      fun pollPeers () =
         if !numConnections >= Constants.desiredConnections then
            pollingTid := Scheduler.once Constants.pollInterval pollPeers
         else
            pollLoop 0
            
      and pollLoop failures = 
         (case Peer.next failures of
             NONE =>
                (
                Log.long (fn () => "No more known peers to contact");
                pollingTid := Scheduler.once Constants.pollInterval pollPeers
                )
           | SOME peer =>
                (
                Log.long (fn () => "Contacting peer");
                openConn peer
                   (fn () =>
                       (* We might have gotten another connection while we waited for this one. *)
                       if !numConnections >= Constants.desiredConnections then
                          pollingTid := Scheduler.once Constants.pollInterval pollPeers
                       else
                          pollLoop (failures+1))
                   (fn conn =>
                       (
                       pollingTid := Scheduler.once Constants.pollInterval pollPeers;
                       !theConnCallback conn
                       ))
                ))

      fun suspendPolling () = Scheduler.cancel (!pollingTid)

      fun resumePolling () =
         pollingTid := Scheduler.onceAbs Time.zeroTime pollPeers



      fun sendMessage (conn as {sock, opn, ...}:conn) msg =
         if !opn then
            if Network.sendVec (sock, BS.full (Message.writeMessage msg)) then
               ()
            else
               closeConn conn false
         else
            ()


      fun broadcastMessage msg =
         let
            val str = BS.full (Message.writeMessage msg)
         in
            app (fn conn as {sock, opn, ...}:conn =>
                    if !opn then
                       if Network.sendVec (sock, str) then
                          ()
                       else
                          closeConn conn false
                    else
                       ()) (!allConnections)
         end

      fun broadcastPing () =
         (
         Log.short "(ping)";
         broadcastMessage (M.Ping (ConvertWord.intInfToWord64 (MTRand.randBits 64)))
         )
            


      fun numberOfConnections () = !numConnections


      fun initialize connCallback msgCallback =
         (
         theConnCallback := connCallback;
         theMsgCallback := msgCallback;
         allConnections := [];
         numConnections := 0;
         Scheduler.repeating Constants.reapInterval reapIdleConnections;
         pollingTid := Scheduler.once Time.zeroTime pollPeers;
         Scheduler.repeating Constants.keepAliveInterval broadcastPing;
         ()
         )


      fun eq ({opn=r, ...}:conn, {opn=r', ...}:conn) = r = r'

      fun lastBlock ({lastBlock, ...}:conn) = lastBlock

      fun orphanage ({orphanage, ...}:conn) = orphanage

   end
