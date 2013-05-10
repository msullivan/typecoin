
structure Message :> MESSAGE =
   struct

      structure B = Bytestring
      structure BS = Bytesubstring
      structure W = Writer
      structure R = Reader
      
      type word8 = Word8.word
      type word32 = Word32.word
      type word64 = Word64.word

      fun >>= (r, f) = R.bind r f
      fun >> (r, r') = R.seq r r'
      fun >>> (w, w') = W.seq w w'
      infixr 3 >>= >> >>>



      (* constants *)
      val serviceNetwork : Word64.word = 0w1

      val theServices : word64 = serviceNetwork
      val okVersion = 31402  (* v.31402 changed the format of addr *)


      (* precomputed values *)
      val magic = ConvertWord.word32ToBytesL Chain.magic
      val zeros = Word8Vector.tabulate (32, (fn _ => 0w0))
      val nullhash = B.substring (zeros, 0, 32)


      fun checksum s =
         B.substring (SHA256.hashBytes (SHA256.hashBytes s), 0, 4)

      fun checksum' s =
         BS.substring (SHA256.hashBytes (SHA256.hash (Stream.fromTable BS.sub s 0)), 0, 4)



      exception InvalidMessage = W.InvalidData



      type netaddr =
         {
         services : word64,
         address : Address.addr,
         port : int
         }

      type version =
         {
         version : int,
         services : word64,
         timestamp : LargeInt.int,
         self : netaddr,
         remote : netaddr,
         nonce : word64,
         agent : string,
         lastBlock : int,
         relay : bool
         }

      type tnetaddr = LargeInt.int * netaddr

      type getblocks =
         {
         version : int,
         hashes : B.string list,
         lastDesired : B.string option
         }
         
      datatype objtp = ERR | TX | BLOCK

      type inv = objtp * Bytestring.string

      type rawalert = Bytestring.string * Bytestring.string

      datatype message =
         Version of version
       | Verack
       | Addr of tnetaddr list
       | Inv of inv list
       | Getdata of inv list
       | Notfound of inv list
       | Getblocks of getblocks
       | Tx of Bytestring.string
       | Block of Bytestring.string
       | Getaddr
       | Ping of Word64.word
       | Pong of Word64.word

       | Alert of rawalert
       | OldVersion of int
       | Unsupported of Bytestring.string
       | Illformed of Bytestring.string



      fun mkNetaddr (addr : Address.addr) =
         { services = theServices, address = addr, port = Chain.port }

      fun writeNetAddr ({ services, address, port }:netaddr) =
         (* services *)
         W.word64L services
         >>>
         (* IPv6/4 *)
         W.bytes (Address.toBitcoin address)
         >>>
         W.word16B (Word.fromInt port)

      val readNetAddr : netaddr R.reader =
         R.word64L
         >>= (fn services =>
         R.wrap (fn str => getOpt (Address.fromBitcoin str, Address.null)) (R.bytesS 16)
         >>= (fn address =>
         R.wrap Word.toInt R.word16B
         >>= (fn port =>
         R.return {services=services, address=address, port=port}
         )))



      fun writeTNetAddr (timestamp, addr) =
         W.word32L (Word32.fromLargeInt timestamp)
         >>>
         writeNetAddr addr

      val readTNetAddr =
         R.wrap Word32.toLargeInt R.word32L
         >>= (fn timestamp =>
         readNetAddr
         >>= (fn addr =>
         R.return (timestamp, addr)
         ))



      fun mkVersion { self, remote, nonce, lastBlock } =
         {
         version = Constants.protocolVersion,
         services = theServices,
         timestamp = Time.toSeconds (Time.now ()),
         self = self,
         remote = remote,
         nonce = nonce,
         agent = Constants.agentName,
         lastBlock = lastBlock,
         relay = true
         }

      fun writeVersion ({version, services, timestamp, self, remote, nonce, agent, lastBlock, relay}:version) =
         W.word32L (Word32.fromInt version)            (* version *)
         >>>
         W.word64L services                            (* services *)
         >>>
         W.word64L (Word64.fromLargeInt timestamp)     (* timestamp *)
         >>>
         writeNetAddr remote                           (* remote *)
         >>>
         writeNetAddr self                             (* self *)
         >>>
         W.word64L nonce                               (* nonce *)
         >>>
         W.bytesVar (B.fromString agent)               (* user agent *)
         >>>
         W.word32L (Word32.fromInt lastBlock)          (* start height *)
         >>>
         (if relay then
             W.null
          else
             W.byte 0w0)                               (* relay *)

      val readVersion =
         R.wrap Word32.toInt R.word32L
         >>= (fn version =>
         if version < okVersion then
            R.return (OldVersion version)
         else
            R.word64L
            >>= (fn services =>
            R.wrap Word64.toLargeInt R.word64L
            >>= (fn timestamp =>
            readNetAddr
            >>= (fn remote =>
            readNetAddr
            >>= (fn self =>
            R.word64L
            >>= (fn nonce =>
            R.wrap B.toString R.bytesVar
            >>= (fn agent =>
            R.wrap Word32.toInt R.word32L
            >>= (fn lastBlock =>
            (if version >= 70001 then
                R.wrap (fn NONE => true | SOME b => b <> 0w0) (R.option R.byte)
             else
                R.return true)
            >>= (fn relay =>
            R.return (Version
                      { version=version, services=services, timestamp=timestamp, remote=remote,
                        self=self, nonce=nonce, agent=agent, lastBlock=lastBlock, relay=relay })
            )))))))))



      fun writeAddr l = W.varlist writeTNetAddr l

      val readAddr = R.wrap Addr (R.varlist readTNetAddr)



      fun mkGetblocks { hashes, lastDesired } =
         { version=Constants.protocolVersion, hashes=hashes, lastDesired=lastDesired }

      fun writeGetblocks { version, hashes, lastDesired } =
         W.word32L (Word32.fromInt version)        (* version *)
         >>>
         W.varlist (W.bytesFixed 32) hashes
         >>>
         (case lastDesired of
             NONE =>
                W.bytes nullhash
           | SOME hash =>
                W.bytesFixed 32 hash)

      val readGetblocks =
         R.wrap Word32.toInt R.word32L
         >>= (fn version =>
         R.varlist (R.bytes 32)
         >>= (fn hashes =>
         R.wrap (fn str => if B.eq (str, nullhash) then NONE else SOME str) (R.bytes 32)
         >>= (fn lastDesired =>
         R.return (Getblocks { version=version, hashes=hashes, lastDesired=lastDesired })
         )))


      
      fun writeObjtp objtp =
         (case objtp of
             ERR => W.word32L 0w0
           | TX => W.word32L 0w1
           | BLOCK => W.word32L 0w2)

      fun writeInv1 (objtp, hash) =
         writeObjtp objtp
         >>>
         W.bytes hash

      fun writeInv l = W.varlist writeInv1 l

      val readObjtp =
         R.wrap (fn 0w0 => ERR | 0w1 => TX | 0w2 => BLOCK | _ => raise R.SyntaxError) R.word32L

      val readInv1 =
         readObjtp
         >>= (fn objtp =>
         R.bytes 32
         >>= (fn hash =>
         R.return (objtp, hash)
         ))

      fun readInv f = R.wrap f (R.varlist readInv1)



      fun writePing nonce = W.word64L nonce
      fun readPing f = R.wrap f R.word64L



      val readAlert =
         R.bytesVar
         >>= (fn payload =>
         R.bytesVar
         >>= (fn sg =>
         R.return (Alert (payload, sg))
         ))

      val parseAlert =
         R.wrap Word32.toInt R.word32L
         >>= (fn version =>
         R.wrap Word64.toLargeInt R.word64L
         >>= (fn relayUntil =>
         R.wrap Word64.toLargeInt R.word64L
         >>= (fn expiration =>
         R.word32L
         >>= (fn id =>
         R.word32L
         >>= (fn cancel =>
         R.varlist R.word32L
         >>= (fn setCancel =>
         R.word32L
         >>= (fn minVer =>
         R.word32L
         >>= (fn maxVer =>
         R.varlist (R.wrap B.toString R.bytesVar)
         >>= (fn setSubVer =>
         R.word32L
         >>= (fn priority =>
         R.wrap B.toString R.bytesVar
         >>= (fn comment =>
         R.wrap B.toString R.bytesVar
         >>= (fn statusBar =>
         R.bytesVar
         >>= (fn reserved =>
         R.return { version=version, relayUntil=relayUntil, expiration=expiration, id=id, cancel=cancel,
                    setCancel=setCancel, minVer=minVer, maxVer=maxVer, setSubVer=setSubVer,
                    priority=priority, comment=comment, statusBar=statusBar, reserved=reserved }
         )))))))))))))




      fun writeAll str = W.bytes str
      fun readAll f = R.wrap (fn x => f (Bytesubstring.string x)) R.all



      fun commandOf str =
         if String.size str > 11 then
            raise (Fail "bad command")
         else
            B.^ (B.fromString str, B.substring (zeros, 0, 12 - String.size str))

      val commandVersion = commandOf "version"
      val commandVerack = commandOf "verack"
      val commandAddr = commandOf "addr"
      val commandInv = commandOf "inv"
      val commandGetdata = commandOf "getdata"
      val commandNotfound = commandOf "notfound"
      val commandGetblocks = commandOf "getblocks"
      val commandTx = commandOf "tx"
      val commandBlock = commandOf "block"
      val commandGetaddr = commandOf "getaddr"
      val commandPing = commandOf "ping"
      val commandPong = commandOf "pong"
      val commandAlert = commandOf "alert"



      fun writePayload (command, payload) =
         let
            val payload' = W.write payload
         in
            W.write
            (W.bytes magic                                  (* magic *)
             >>>
             W.bytes command                                (* command *)
             >>>
             W.word32L (Word32.fromInt (B.size payload'))   (* length *)
             >>>
             W.bytes (checksum payload')                    (* checksum *)
             >>>
             W.bytes payload')                              (* payload *)
         end

      fun writeMessage msg =
         (case msg of
             Version m =>
                writePayload (commandVersion, writeVersion m)
           | Verack =>
                writePayload (commandVerack, W.null)
           | Addr l =>
                   writePayload (commandAddr, writeAddr l)
           | Inv l =>
                writePayload (commandInv, writeInv l)
           | Getdata l =>
                writePayload (commandGetdata, writeInv l)
           | Notfound l =>
                writePayload (commandNotfound, writeInv l)
           | Getblocks m =>
                writePayload (commandGetblocks, writeGetblocks m)
           | Tx str =>
                writePayload (commandTx, writeAll str)
           | Block str =>
                writePayload (commandBlock, writeAll str)
           | Getaddr =>
                writePayload (commandGetaddr, W.null)
           | Ping n =>
                writePayload (commandPing, writePing n)
           | Pong n =>
                writePayload (commandPong, writePing n)

           | Alert _ =>
                raise InvalidMessage
           | OldVersion _ =>
                raise InvalidMessage
           | Unsupported _ =>
                raise InvalidMessage
           | Illformed _ =>
                raise InvalidMessage)



      val commands =
         map (fn (str, f) => (BS.full str, f))
         [(commandVersion, readVersion),
          (commandVerack, R.return Verack),
          (commandAddr, readAddr),
          (commandInv, readInv Inv),
          (commandGetdata, readInv Getdata),
          (commandNotfound, readInv Notfound),
          (commandGetblocks, readGetblocks),
          (commandTx, readAll Tx),
          (commandBlock, readAll Block),
          (commandGetaddr, R.return Getaddr),
          (commandPing, readPing Ping),
          (commandPong, readPing Pong),
          (commandAlert, readAlert)]

      (* size and magic number are already checked *)
      fun readMessage s =
         let
            val msg = BS.slice (s, 24, NONE)
            val command = BS.slice (s, 4, SOME 12)
         in
            if BS.eq (checksum' msg, BS.slice (s, 20, SOME 4)) then
               (case List.find (fn (c, _) => BS.eq (c, command)) commands of
                   SOME (_, r) =>
                      (R.readfull r msg
                       handle
                       R.SyntaxError =>
                          Illformed (BS.string s))
                 | NONE =>
                      Unsupported (BS.string command))
            else
               Illformed (BS.string s)
         end

   end
