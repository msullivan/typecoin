
structure Textcode :> TEXTCODE =
   struct

      structure B = Bytestring

      val secp256k1 = EllipticCurveParams.secp256k1

      exception Invalid = Base58.Invalid

      fun decodeAddressGen prefix string =
         let
            val str = Base58.decode string
         in
            if B.size str <> 21 then
               raise Invalid
            else if B.sub (str, 0) <> prefix then
               raise Invalid
            else
               B.substring (str, 1, B.size str - 1)
         end

      fun encodeAddressGen prefix str =
         Base58.encode (B.^ (B.str prefix, str))

      val decodeAddress = decodeAddressGen 0w0
      val encodeAddress = encodeAddressGen 0w0

      val decodeAddressTestnet = decodeAddressGen 0w111
      val encodeAddressTestnet = encodeAddressGen 0w111

      fun decodePrivkeyGen prefix string =
         let
            val privkeystr = Base58.decode string
      
            val (privkeystr', compressed) =
               if B.size privkeystr < 1 then
                  raise Invalid
               else if B.sub (privkeystr, 0) <> prefix then
                  raise Invalid
               else if B.size privkeystr = 34 then
                  if B.sub (privkeystr, 33) <> 0w1 then
                     raise Invalid
                  else
                     (B.substring (privkeystr, 1, 32), true)
               else if B.size privkeystr = 33 then
                  (B.substring (privkeystr, 1, 32), false)
               else
                  raise (Fail "bad length")
      
            val privkey = ConvertIntInf.fromBytesB privkeystr'

            val () =
               if EllipticCurveCryptoFp.validPrivkey (secp256k1, privkey) then
                  ()
               else
                  raise Invalid
         in
            (privkey, compressed)
         end

      fun encodePrivkeyGen prefix (privkey, compressed) =
         Base58.encode
         (B.concat [B.str prefix, ConvertIntInf.toFixedBytesB (32, privkey), if compressed then B.str 0w1 else B.null])

      val decodePrivkey = decodePrivkeyGen 0w128
      val encodePrivkey = encodePrivkeyGen 0w128

      val decodePrivkeyTestnet = decodePrivkeyGen 0w239
      val encodePrivkeyTestnet = encodePrivkeyGen 0w239

   end
