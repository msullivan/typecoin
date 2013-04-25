
structure Commo :> COMMO =
   struct

      structure B = Bytestring
      structure BS = Bytesubstring
      structure M = Message


      (* constants *)
      val serviceNetwork : Word64.word = 0w1
      val peerUpdateThreshold = Time.fromSeconds (5 * 60)  (* 5 minutes *)

      (* precomputed values *)
      val netAddrNull = M.mkNetaddr Address.null
      val magic = BS.full (ConvertWord.word32ToBytesL Chain.magic)
      val msgVerack = M.writeMessage M.Verack


      fun recvMessage k =
         let
            fun k' str =
               if BS.eq (BS.slice (str, 0, SOME 4), magic) then
                  let
                     val sz = Word32.toInt (ConvertWord.bytesToWord32SL (BS.slice (str, 16, SOME 4)))
                  in
                     Sink.MORE (sz, (fn str' => k (M.readMessage (BS.full (BS.concat [str, str'])))))
                  end
               else
                  (* bad magic number *)
                  Sink.DONE
         in
            Sink.MORE (24, k')
         end


      (* Connect to addr, handshake, then return the sock to k, which must provide a sink. *)
      fun contact addr port k =
         let
            val (sock, already) = Network.connectNB (Address.toInAddr addr, port)

            fun handshake () =
               let
                  val () = print "Connected to ";
                  val () = print (Address.toString addr)
                  val () = print "\n"
               
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
      
                  fun k' msg =
                     (case msg of
                         M.Version {services, ...} =>
                            if Word64.andb (services, serviceNetwork) = 0w0 then
                               (* Ignore peers that don't offer the network service.  If there were a lot
                                  of them, it might be better to use them for the services they do offer.
                               *)
                               Sink.DONE
                            else
                               recvMessage
                               (fn M.Verack =>
                                      (
                                      Network.sendVec (sock, BS.full msgVerack);
                                      k sock
                                      )
                                 | _ =>
                                      (* Did not handshake correctly. *)
                                      Sink.DONE)
                       | _ =>
                            (* Did not handshake correctly. *)
                            Sink.DONE)
      
               in
                  (* Send version, expect version and verack, then send verack. *)
                  Network.sendVec (sock, BS.full msgVersion);
                  Sink.register sock (recvMessage k')
               end
         in
            if already then
               (* Does this ever happen? *)
               handshake ()
            else
               Scheduler.insertWrite sock
               (fn () =>
                   (
                   Scheduler.delete sock;
                   handshake ()
                   ))
         end



      type conn = 
         {
         sock : Network.asock,
         peer : Peer.peer
         }

      val theCallback : (conn * Message.message -> unit) ref = ref (fn _ => ())

      fun initialize callback =
         (
         theCallback := callback
         )

      fun process (conn as ({peer, ...}:conn)) msg =
         let
            val time = Time.now ()
         in
            (* If we haven't updated the peer since the threshold time, do so. *)
            if Time.>= (time, Time.+ (Peer.time peer, peerUpdateThreshold)) then
               Peer.update peer time
            else
               ();
   
            !theCallback (conn, msg)
         end



      fun openConn peer k =
         contact (Peer.address peer) Chain.port
         (fn sock =>
             let
                val conn = { sock=sock, peer=peer }

                fun loop () =
                   recvMessage
                   (fn msg =>
                       (
                       process conn msg;
                       loop ()
                       ))
             in
                Peer.update peer (Time.now ());
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

   end
