
structure Address :> ADDRESS =
   struct

      type addr = Word32.word

      val null : addr = 0w0

      fun eq (x:addr, y) = x = y

      val compare = Word32.compare

      structure Ordered =
         struct
            type t = addr
            val eq = eq
            val compare = Word32.compare
         end

      structure Hashable =
         struct
            type t = addr
            val eq = eq
            val hash = Platform.hashWord32
         end

      fun toList addr =
         Bytestring.explode (ConvertWord.word32ToBytesB addr)

      fun fromList l =
         (SOME (ConvertWord.bytesToWord32B (Bytestring.implode l))
          handle ConvertWord.ConvertWord => NONE)

      fun toString addr =
         String.concatWith "." (map (Int.toString o Word8.toInt) (toList addr))

      exception BadField

      fun fromString str =
         fromList
         (map (fn str => (case FromString.toInt str of
                             SOME x => Word8.fromInt x
                           | NONE => raise BadField))
          (String.fields (fn ch => ch = #".") str))
         handle BadField => NONE

      fun toInAddr addr =
         valOf (NetHostDB.fromString (toString addr))

      fun fromInAddr inaddr =
         valOf (fromString (NetHostDB.toString inaddr))

      val ipV4Prefix =
         valOf (Bytestring.fromStringHex "00000000000000000000ffff")
         
      fun toBitcoin addr =
         Bytestring.^ (ipV4Prefix, ConvertWord.word32ToBytesB addr)

      fun fromBitcoin str =
         if Bytesubstring.size str = 16
            andalso
            Bytesubstring.eq (Bytesubstring.slice (str, 0, SOME 12), Bytesubstring.full ipV4Prefix)
         then
            SOME (ConvertWord.bytesToWord32SB (Bytesubstring.slice (str, 12, NONE)))
         else
            NONE

   end
