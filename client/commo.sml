
signature CONN_STATE =
   sig
      type state
      val new : unit -> state
   end


functor CommoFun (structure ConnState : CONN_STATE)
   :>
   COMMO 
   where type state = ConnState.state
   =
   struct

      structure B = Bytestring
      structure BS = Bytesubstring
      structure M = Message


      (* precomputed values *)
      val netAddrNull = M.mkNetaddr Address.null
      val magic = BS.full (ConvertWord.word32ToBytesL Chain.magic)
      val msgVerack = M.writeMessage M.Verack



      (* Attempts to receive a message.  If it succeeds, invokes sk with the message, which must return
         a new sink.  If the connection is interrupted before it can process a message, invokes fk.
      *)
      fun recvMessage fk sk =
         let
            fun k str =
               if BS.eq (BS.slice (str, 0, SOME 4), magic) then
                  let
                     val sz = Word32.toInt (ConvertWord.bytesToWord32SL (BS.slice (str, 16, SOME 4)))
                  in
                     if sz > Constants.maximumPayload then
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


      (* Cache this. *)
      val myNetaddr = ref (M.mkNetaddr Address.null)
      val myAddrIsPrivate = ref true

      fun setOwnAddress () =
         let
            val addr =
               (case Network.self () of
                   [] => Address.null
                 | iaddr :: _ => Address.fromInAddr iaddr)
         in
            myAddrIsPrivate :=
               (case Address.toList addr of
                   [0w0, 0w0, 0w0, 0w0] => true
                 | [0w10, _, _, _] => true
                 | [0w172, b, _, _] => 0w16 <= b andalso b < 0w32
                 | [0w192, 0w168, _, _] => true
                 | _ => false);

            myNetaddr := M.mkNetaddr addr
         end



      (* Handshake, then invoke sk (giving it the remote's version), which much provide a sink.
         If handshaking fails, invoke fk.
      *)
      fun handshakeOut addr sock fk sk =
         let
            val tid =
               Scheduler.once Constants.connectTimeout
               (fn () =>
                   (
                   Log.long (fn () => "Timeout handshaking with " ^ Address.toString addr);
                   Scheduler.delete sock;
                   Network.close sock;
                   fk ()
                   ))

            fun fk' () =
               (
               Scheduler.cancel tid;
               Log.long (fn () => "Handshake failed with " ^ Address.toString addr);
               fk ()
               )

            val nonce = ConvertWord.bytesToWord64B (AESFortuna.random 8)

            val msgVersion =
               M.writeMessage
                  (M.Version
                      (M.mkVersion
                          {
                          self = !myNetaddr,
                          remote = M.mkNetaddr addr,
                          nonce = nonce,
                          lastBlock = Blockchain.lastBlock ()
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
                                   Scheduler.cancel tid;
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

      
      (* Handshake, then invoke sk (giving it the remote's version), which much provide a sink.
         If handshaking fails, invoke fk.
      *)
      fun handshakeIn addr sock fk sk =
         let
            val tid =
               Scheduler.once Constants.connectTimeout
               (fn () =>
                   (
                   Log.long (fn () => "Timeout handshaking with incoming " ^ Address.toString addr);
                   Scheduler.delete sock;
                   Network.close sock;
                   fk ()
                   ))

            fun fk' () =
               (
               Scheduler.cancel tid;
               Log.long (fn () => "Incoming handshake failed with " ^ Address.toString addr);
               fk ()
               )

            fun k msg =
               (case msg of
                   M.Version (ver as {services, ...}) =>
                      if Word64.andb (services, Message.serviceNetwork) = 0w0 then
                         (* Ignore peers that don't offer the network service. Maybe we shouldn't
                            do this for incoming connections.
                         *)
                         (fk' (); Sink.DONE)
                      else
                         let
                            val nonce = ConvertWord.bytesToWord64B (AESFortuna.random 8)
                
                            val msgVersion =
                               M.writeMessage
                                  (M.Version
                                      (M.mkVersion
                                          {
                                          self = !myNetaddr,
                                          remote = M.mkNetaddr addr,
                                          nonce = nonce,
                                          lastBlock = Blockchain.lastBlock ()
                                          }))
                         in
                            if not (Network.sendVec (sock, BS.full msgVersion)) then
                               (fk' (); Sink.DONE)
                            else if not (Network.sendVec (sock, BS.full msgVerack)) then
                               (fk' (); Sink.DONE)
                            else
                               recvMessage fk'
                               (fn M.Verack =>
                                      let in
                                         Scheduler.cancel tid;
                                         Log.long (fn () => "Incoming handshake successful with " ^ Address.toString addr);
                                         sk ver
                                      end
                                 | _ =>
                                      (* Did not handshake correctly. *)
                                      (fk' (); Sink.DONE))
                         end
                 | _ =>
                      (* Did not handshake correctly. *)
                      (fk' (); Sink.DONE))
         in
            (* Expect version, send version and verack, then expect verack. *)
            Sink.register sock (recvMessage fk' k)
         end



      type state = ConnState.state

      type conn = 
         {
         sock : Network.asock,
         peer : Peer.peer,
         lastHeard : Time.time ref,  (* zero indicates forcibly closed *)
         opn : bool ref,             (* still open?  also used for quick equality test *)
         lastBlock : int,
         state : state
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


      fun initializeConn peer sock ({lastBlock, ...}:M.version) =
         let
            val lastHeard = ref (Time.now ())
            val conn = { sock=sock, peer=peer, lastHeard=lastHeard, opn=ref true,
                         lastBlock=lastBlock, state=ConnState.new () }

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
            !theConnCallback conn;
            loop ()
         end
      
         
      fun contact peer fk sk =
         let
            val addr = Peer.address peer
            val (sock, already) = Network.connectNB (Address.toInAddr addr, Chain.port)

            fun fk' () =
               (
               (* Couldn't contact this peer.  Delete it from the peer set. *)
               Peer.delete peer;
               fk ()
               )

            fun sk' (sock, ver) =
               (
               sk ();
               initializeConn peer sock ver
               )
         in
            if already then
               (* Does this ever happen? *)
               handshakeOut addr sock fk' sk'
            else
               let
                  val tid =
                     Scheduler.once Constants.connectTimeout
                     (fn () =>
                         (
                         Log.long (fn () => "Timeout connecting to " ^ Address.toString addr);
                         Scheduler.delete sock;
                         Network.close sock;
                         fk' ()
                         ))
               in
                  Scheduler.insertWrite sock
                  (fn () =>
                      (
                      Scheduler.cancel tid;
                      Scheduler.delete sock;
                      handshakeOut addr sock fk' sk'
                      ))
               end
         end


      fun answer insock =
         let
            val (sock, iaddr, port) = Network.accept insock
            val addr = Address.fromInAddr iaddr
         in
            handshakeIn addr sock
            (fn () => ())
            (fn ver => initializeConn (Peer.degenerate addr) sock ver)
         end



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
                let in
                   Log.long (fn () => "Contacting peer");
   
                   contact peer
                   (fn () =>
                       (* We might have gotten another connection while we waited for this one.
                          If not, poll with failure count incremented.
                       *)
                       if !numConnections >= Constants.desiredConnections then
                          pollingTid := Scheduler.once Constants.pollInterval pollPeers
                       else
                          pollLoop (failures+1))
                   (fn () =>
                       pollingTid := Scheduler.once Constants.pollInterval pollPeers)
                end)

      fun suspendPolling () = Scheduler.cancel (!pollingTid)

      fun resumePolling () =
         pollingTid := Scheduler.onceAbs Time.zeroTime pollPeers


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
            

      val theInsock : Network.psock option ref = ref NONE

      fun initialize connCallback msgCallback =
         let in
            theConnCallback := connCallback;
            theMsgCallback := msgCallback;
            allConnections := [];
            numConnections := 0;

            (let
                val insock = Network.listen Chain.port
             in
                theInsock := SOME insock;
                Scheduler.insertRead insock (fn () => answer insock)
             end
             handle NetworkException =>
                let in
                   Log.long (fn () => "Unable to open socket for incoming connections");
                   theInsock := NONE
                end);

            Scheduler.repeating Constants.reapInterval reapIdleConnections;
            pollingTid := Scheduler.once Time.zeroTime pollPeers;
            Scheduler.repeating Constants.keepAliveInterval broadcastPing;

            setOwnAddress ();
            (* Your IP address can change, so check it periodically. *)
            Scheduler.repeating Constants.getOwnAddressInterval setOwnAddress;

            ()
         end

      fun cleanup () =
         let in
            Option.app Network.close (!theInsock);
            theInsock := NONE
         end


      fun numberOfConnections () = !numConnections

      fun self () =
         if !myAddrIsPrivate then
            NONE
         else
            SOME (!myNetaddr)

      fun closed ({opn, ...}:conn) = not (!opn)

      fun eq ({opn=r, ...}:conn, {opn=r', ...}:conn) = r = r'

      fun lastBlock ({lastBlock, ...}:conn) = lastBlock

      fun state ({state, ...}:conn) = state

      fun peer ({peer, ...}:conn) = peer

   end


structure Commo =
   CommoFun
   (structure ConnState =
       struct
          type state =
             { orphanage : Blockchain.orphanage,
               trigger : Bytestring.string ref }

          fun new () =
             { orphanage = Blockchain.newOrphanage (),
               trigger = ref Bytestring.null }
       end)
