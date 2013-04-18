
structure Protocol :> PROTOCOL =
   struct

      structure B = Bytestring
      structure W = Writer
      
      type word8 = Word8.word
      type word32 = Word32.word
      type word64 = Word64.word


      type ipaddr = word8 list  (* length 4 *)

      type netaddr =
         {
         services : word64,
         address : ipaddr,
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
         lastBlock : int
         }

      exception InvalidMessage = W.InvalidData

      (* constants *)
      val magicTestnet3 : word32 = 0wx0709110b
      val portTestnet = 18333
      val theVersion = 70001 
      val theServices : word64 = 0w1
      val theAgent = "/Satoshi:0.8.1/"

      fun checksum s =
         B.substring (SHA256.hashBytes (SHA256.hashBytes s), 0, 4)


      fun mkNetaddr addr =
         { services = theServices, address = addr, port = portTestnet }

      fun mkVersion { self, remote, nonce, lastBlock } =
         {
         version = theVersion,
         services = theServices,
         timestamp = Time.toSeconds (Time.now ()),
         self = self,
         remote = remote,
         nonce = nonce,
         agent = theAgent,
         lastBlock = lastBlock
         }
      


      fun writeNetAddr ({ services, address, port }:netaddr) =
         if length address <> 4 then
            raise InvalidMessage
         else
            W.seql
            [
            (* services *)
            W.word64L services,
            (* IPv6/4 *)
            W.repeat 10 (W.byte 0w0), W.byte 0wxff, W.byte 0wxff, W.seql (map W.byte address),
            (* port *)
            W.word16B (Word.fromInt port)
            ]

      fun writeVersion ({version, services, timestamp, self, remote, nonce, agent, lastBlock}:version) =
         W.seql
         [
         (* version *)
         W.word32L (Word32.fromInt version),
         (* services *)
         W.word64L services,
         (* timestamp *)
         W.word64L (Word64.fromLargeInt timestamp),
         (* remote *)
         writeNetAddr remote,
         (* self *)
         writeNetAddr self,
         (* nonce *)
         W.word64L nonce,
         (* user agent *)
         W.bytesVar (B.fromString agent),
         (* start height *)
         W.word32L (Word32.fromInt lastBlock)
         ]

      fun writePayload (command, payload) =
         let
            val payload' = W.write payload
         in
            W.seql
            [
            (* magic *)
            W.word32L magicTestnet3,
            (* command *)
            W.bytesPad 11 (B.fromString command), W.byte 0w0,
            (* length *)
            W.word32L (Word32.fromInt (B.size payload')),
            (* checksum *)
            W.bytes (checksum payload'),
            (* payload *)
            W.bytes payload'
            ]
         end

      datatype message =
         Version of version
       | Unsupported of string

      exception InvalidMessage

      fun writeMessage msg =
         (case msg of
             Version version =>
                writePayload ("version", writeVersion version)
           | Unsupported str =>
                raise InvalidMessage)

   end
