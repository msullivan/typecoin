
signature PROTOCOL =
   sig

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

      val mkNetaddr : ipaddr -> netaddr
      val mkVersion : { self : netaddr, remote : netaddr, nonce : word64, lastBlock : int } -> version
      
      datatype message =
         Version of version
       | Unsupported of string

      exception InvalidMessage

      val writeMessage : message -> Writer.writer

   end
