
structure Commo :> COMMO =
   struct

      structure B = Bytestring
      structure BS = Bytesubstring
      structure M = Message


      (* constants *)

      (* Update a peer's timestamp at most this often. *)
      val peerUpdateThreshold = Time.fromSeconds (5 * 60)  (* 5 minutes *)

      (* When this long for a connection to go through. *)
      val connectTimeout = Time.fromSeconds 30             (* 30 seconds *)

      (* Connections become reapable when idle this long. *)
      val idleTimeout = Time.fromSeconds 60                (* 1 minute, XX would typically be larger *)

      (* Reap reapable connections this often. *)
      val reapInterval = Time.fromSeconds 60               (* 1 minute *)

      val maximumPayload = 1800000                         (* 1.8 million bytes *)

      (* Contact as many as this many peers at once when connections aren't going through. *)
      val batchLimit = 8


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

            fun handshake () =
               let
                  val () = Log.long (fn () => "Connected to " ^ Address.toString addr)
               
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
                               (fk (); Sink.DONE)
                            else
                               recvMessage fk
                               (fn M.Verack =>
                                      (
                                      Network.sendVec (sock, BS.full msgVerack);
                                      sk (sock, ver)
                                      )
                                 | _ =>
                                      (* Did not handshake correctly. *)
                                      (fk (); Sink.DONE))
                       | _ =>
                            (* Did not handshake correctly. *)
                            (fk (); Sink.DONE))
      
               in
                  (* Send version, expect version and verack, then send verack. *)
                  Network.sendVec (sock, BS.full msgVersion);
                  Sink.register sock (recvMessage fk k)
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
                         Log.long (fn () => "Timeout connecting to " ^ Address.toString addr);
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
         opn : bool ref,             (* still open?  also used for quick equality test *)

         (* Data held for other modules.  If there's too many of these, we should functorize over them. *)
         lastBlock : int,
         orphanage : Blockchain.orphanage
         }

      val theCallback : (conn * Message.message -> unit) ref = ref (fn _ => ())
      val allConnections : conn list ref = ref []


      fun closeConn ({sock, peer, opn, ...}:conn) withPrejudice =
         if !opn then
            (
            Network.tryClose sock;
            Scheduler.delete sock;
            opn := false;
            if withPrejudice then
               (* Closed with prejudice, don't re-enqueue it. *)
               Peer.delete peer
            else
               Peer.enqueue peer
            )
         else
            (* already closed *)
            ()


      fun reapIdleConnections () =
         let
            val () = Log.long (fn () => "Reaping")
            val cutoff = Time.- (Time.now (), idleTimeout)
         in
            allConnections
               := 
               foldl (fn (conn as {sock, peer, lastHeard, opn, ...}:conn, l) =>
                         if Time.>= (!lastHeard, cutoff) then
                            conn :: l
                         else 
                            (
                            Log.long (fn () => "Disconnecting " ^ Address.toString (Peer.address peer));
                            closeConn conn false;
                            l)) [] (!allConnections)
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


      val batch = ref 1

      fun openConn peer k =
         contact (Peer.address peer) Chain.port
         (fn () =>
             (* Couldn't contact this peer.  Delete it from the peer set, and add
                one to the next batch to make up for it.
             *)
             (
             Peer.delete peer;
             if !batch < batchLimit then
                batch := !batch + 1
             else
                ()
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
                allConnections := conn :: !allConnections;
                k conn;
                loop ()
             end)

      fun openConns k =
         let
            fun loop n =
               if n <= 0 then
                  ()
               else
                  (case Peer.next () of
                      NONE => ()
                    | SOME peer =>
                         (
                         openConn peer k;
                         loop (n-1)
                         ))

            val n = !batch
         in
            batch := 1;
            Log.long (fn () => if n = 1 then "Adding peer" else "Adding " ^ Int.toString n ^ " peers");
            loop n
         end



      fun sendMessage' (conn as {sock, opn, ...}:conn) msg =
         if !opn then
            let
               val success =
                  Network.sendVec (sock, BS.full (Message.writeMessage msg))
            in
               if success then
                  ()
               else
                  closeConn conn true;

               success
            end
         else
            false

      fun sendMessage conn msg = (sendMessage' conn msg; ())



      fun initialize callback =
         (
         theCallback := callback;
         allConnections := [];
         batch := 1;
         Scheduler.repeating reapInterval reapIdleConnections;
         ()
         )


      fun eq ({opn=r, ...}:conn, {opn=r', ...}:conn) = r = r'

      fun lastBlock ({lastBlock, ...}:conn) = lastBlock

      fun orphanage ({orphanage, ...}:conn) = orphanage

   end
