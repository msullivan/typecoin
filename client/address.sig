
signature ADDRESS =
   sig

      type addr

      structure Ordered : ORDERED where type t = addr
      structure Hashable : HASHABLE where type t = addr

      val null : addr
      val eq : addr * addr -> bool

      val toInAddr : addr -> NetHostDB.in_addr
      val fromInAddr : NetHostDB.in_addr -> addr
      val toString : addr -> string
      val fromString : string -> addr option
      val toList : addr -> Word8.word list
      val fromList : Word8.word list -> addr option
      val toBitcoin : addr -> Bytestring.string
      val fromBitcoin : Bytesubstring.substring -> addr option

   end
