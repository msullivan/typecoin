
structure Protocol :> PROTOCOL =
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


      type buffer = (int * BS.substring list) ref
      type bufsock = Network.asock * buffer

      exception NoMessage  (* I/O error, socket closed, or bad magic number *)

      fun recvMessage (sock, bufr) =
         let
            (* accumulate enough data to read the message *)
            fun messageloop (sz, n, buf) =
               if n >= sz then
                  let
                     val str = BS.concat (rev buf)
                  in
                     bufr := (n - sz, [BS.extract (str, sz, NONE)]);
                     M.readMessage (BS.substring (str, 0, sz))
                  end
               else
                  let
                     val v = Network.recvVec sock
                     val m = B.size v
                  in
                     if m = 0 then
                        (* socket closed *)
                        raise NoMessage
                     else
                        messageloop (sz, n+m, BS.full v :: buf)
                  end

            (* accumulate enough data to read the header (normally will only take one packet) *)
            fun headerloop (n, buf) =
               if n >= 24 then
                  let
                     val str = BS.concat (rev buf)
                  in
                     if BS.eq (BS.substring (str, 0, 4), magic) then
                        let
                           val psz = Word32.toInt (ConvertWord.bytesToWord32SL (BS.substring (str, 16, 4)))
                        in
                           messageloop (psz+24, n, [BS.full str])
                        end
                     else
                        (* bad magic number *)
                        raise NoMessage
                  end
               else
                  let
                     val v = Network.recvVec sock
                     val m = B.size v
                  in
                     if m = 0 then
                        (* socket closed *)
                        raise NoMessage
                     else
                        headerloop (n+m, BS.full v :: buf)
                  end
         in
            headerloop (!bufr)
            handle OS.SysErr _ => raise NoMessage
         end

         

      fun handshake addr =
         let
            val sock = Network.connect (addr, Chain.port)
            val ss : bufsock = (sock, ref (0, []))

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
         in
            Network.sendVec (sock, BS.full msgVersion);
            (case recvMessage ss of
                M.Version {services, ...} =>
                   if Word64.andb (services, serviceNetwork) = 0w0 then
                      (* Ignore peers that don't offer the network service.  If there were a lot
                         of them, it might be better  to use them for the services they do offer.
                      *)
                      NONE
                   else
                      (case recvMessage ss of
                          M.Verack =>
                             (
                             Network.sendVec (sock, BS.full msgVerack);
                             SOME ss
                             )
                        | _ =>
                             NONE)
              | _ => NONE)
         end
         handle NoMessage => NONE

   end
