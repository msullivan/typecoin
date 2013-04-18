
structure Script :> SCRIPT =
   struct

      datatype inst =
         Const of Bytestring.string
       | Dup
       | Hash160
       | Equalverify
       | Checksig


      structure B = Bytestring

      fun renderInst i =
         (case i of
             Const s =>
                let
                   val sz = B.size s
                in
                   if sz <= 0x4b then
                      B.^ (B.str (Word8.fromInt sz), s)
                   else
                      raise (Fail "large constant unimplemented")
                end
           | Dup => B.str 0wx76
           | Equalverify => B.str 0wx88
           | Hash160 => B.str 0wxa9
           | Checksig => B.str 0wxac)

      fun render l = B.concat (map renderInst l)

   end
