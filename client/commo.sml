
structure Commo :> COMMO =
   struct

      structure B = Bytestring
      structure BS = Bytesubstring
      structure M = Message


      (* constants *)
      val serviceNetwork : Word64.word = 0w1

      (* precomputed values *)
      val netAddrNull = M.mkNetaddr (M.V4 [0w0, 0w0, 0w0, 0w0])
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
      fun contact addr k =
         let
            val sock = Network.connect (addr, Chain.port)

            val nonce = ConvertWord.bytesToWord64B (AESFortuna.random 8)

            val msgVersion =
               M.writeMessage
                  (M.Version
                      (M.mkVersion
                          {
                          self = netAddrNull,
                          remote = M.mkNetaddr (M.V4 (Network.explodeAddr addr)),
                          nonce = nonce,
                          lastBlock = 0
                          }))

            fun k' msg =
               (case msg of
                   M.Version {services, ...} =>
                      if Word64.andb (services, serviceNetwork) = 0w0 then
                         (* Ignore peers that don't offer the network service.  If there were a lot
                            of them, it might be better  to use them for the services they do offer.
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


      type peer = Network.asock * unit

      val theCallback : (peer * Message.message -> unit) ref = ref (fn _ => ())

      fun initialize callback =
         theCallback := callback;



      fun process peer msg =
         (* Need to do much more. *)
         !theCallback (peer, msg)


      fun addPeer addr k =
         contact addr
         (fn sock =>
             let
                val peer = (sock, ())

                fun loop () =
                   recvMessage
                   (fn msg =>
                       (
                       process peer msg;
                       loop ()
                       ))
             in
                k peer;
                loop ()
             end)


      fun sendMessage (sock, _) msg =
         let
            val success =
               Network.sendVec (sock, BS.full (Message.writeMessage msg))
         in
            if success then
               ()
            else
               (
               Network.tryClose sock;
               Scheduler.deleteSock sock
               );
            success
         end

   end
