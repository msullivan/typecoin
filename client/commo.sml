
structure Commo :> COMMO =
   struct

      structure B = Bytestring
      structure BS = Bytesubstring
      structure M = Message


      (* constants *)

      (* Update a peer's timeout at most this often. *)
      val peerUpdateThreshold = Time.fromSeconds (5 * 60)  (* 5 minutes *)

      (* When this long for a connection to go through. *)
      val connectTimeout = Time.fromSeconds 30             (* 30 seconds *)

      (* Connections become reapable when idle this long. *)
      val idleTimeout = Time.fromSeconds 60                (* 1 minute, XX would typically be larger *)

      (* Reap reapable connections this often. *)
      val reapInterval = Time.fromSeconds 60               (* 1 minute *)

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
                        fk ()
                     else
                        Sink.MORE (sz, (fn str' => sk (M.readMessage (BS.full (BS.concat [str, str'])))))
                  end
               else
                  (* bad magic number *)
                  fk ()
         in
            Sink.MORE (24, k)
         end


      (* Connect to addr, handshake, then return the sock to sk, which must provide a sink.
         If connection or handshaking fails, invoke fk.
      *)
      fun contact addr port fk sk =
         let
            val (sock, already) = Network.connectNB (Address.toInAddr addr, port)

            fun handshake () =
               let
                  val () = Log.log (fn () => "Connected to " ^ Address.toString addr ^ "\n")
               
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
      
                  fun fsink () = (fk (); Sink.DONE)

                  fun k msg =
                     (case msg of
                         M.Version (ver as {services, ...}) =>
                            if Word64.andb (services, Message.serviceNetwork) = 0w0 then
                               (* Ignore peers that don't offer the network service.  If there were a lot
                                  of them, it might be better to use them for the services they do offer.
                               *)
                               fsink ()
                            else
                               recvMessage fsink
                               (fn M.Verack =>
                                      (
                                      Network.sendVec (sock, BS.full msgVerack);
                                      sk (sock, ver)
                                      )
                                 | _ =>
                                      (* Did not handshake correctly. *)
                                      fsink ())
                       | _ =>
                            (* Did not handshake correctly. *)
                            fsink ())
      
               in
                  (* Send version, expect version and verack, then send verack. *)
                  Network.sendVec (sock, BS.full msgVersion);
                  Sink.register sock (recvMessage fsink k)
               end
         in
            if already then
               (* Does this ever happen? *)
               handshake ()
            else
               let
                  val tid =
                     Scheduler.once connectTimeout
                     (fn () =>
                         (
                         Log.log (fn () => "Failed to connect to " ^ Address.toString addr ^ "\n");
                         Network.tryClose sock;
                         Scheduler.delete sock;
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
         lastBlock : int
         }

      val theCallback : (conn * Message.message -> unit) ref = ref (fn _ => ())
      val allConnections : conn list ref = ref []



      fun reapIdleConnections () =
         let
            val () = Log.log (fn () => "Reaping\n")
            val cutoff = Time.- (Time.now (), idleTimeout)
         in
            allConnections
               := foldl (fn (conn as {sock, peer, lastHeard, ...}:conn, l) =>
                            if Time.>= (!lastHeard, cutoff) then
                               conn :: l
                            else if !lastHeard = Time.zeroTime then
                               (* Connection has been forcibly closed.  The socket has already been
                                  closed and the peer deleted.  Don't want to enqueue it.
                               *)
                               l
                            else
                               (
                               Log.log (fn () => "Disconnecting " ^ Address.toString (Peer.address peer) ^ "\n");
                               Peer.enqueue peer;
                               Network.tryClose sock;
                               Scheduler.delete sock;
                               l
                               )) [] (!allConnections)
         end


      fun processMessage (conn as ({peer, lastHeard, ...}:conn)) msg =
         let
            val time = Time.now ()
         in
            lastHeard := time;

            (* If we haven't updated the peer since the threshold time, do so. *)
            if Time.>= (time, Time.+ (Peer.time peer, peerUpdateThreshold)) then
               Peer.update peer time
            else
               ();
   
            !theCallback (conn, msg)
         end


      fun openConn peer k =
         contact (Peer.address peer) Chain.port
         (fn () => Peer.delete peer)
         (fn (sock, {lastBlock, ...}:M.version) =>
             let
                val lastHeard = ref (Time.now ())
                val conn = { sock=sock, peer=peer, lastHeard=lastHeard, lastBlock=lastBlock }

                fun loop () =
                   recvMessage
                   (fn () =>
                       (
                       Peer.delete peer;
                       lastHeard := Time.zeroTime;
                       Sink.DONE
                       ))
                   (fn msg =>
                       (
                       processMessage conn msg;
                       loop ()
                       ))
             in
                Peer.update peer (Time.now ());
                allConnections := conn :: !allConnections;
                k conn;
                loop ()
             end)


      fun sendMessage ({sock, ...}:conn) msg =
         let
            val success =
               Network.sendVec (sock, BS.full (Message.writeMessage msg))
         in
            if success then
               ()
            else
               (
               Network.tryClose sock;
               Scheduler.delete sock
               );
            success
         end



      fun initialize callback =
         (
         theCallback := callback;
         allConnections := [];
         Scheduler.repeating reapInterval reapIdleConnections;
         ()
         )


      fun lastBlock ({lastBlock, ...}:conn) = lastBlock

   end
