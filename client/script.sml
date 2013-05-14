
structure Script :> SCRIPT =
   struct

      structure B = Bytestring
      structure W = Writer
      structure R = Reader

      fun >>= (r, f) = R.bind r f
      fun >> (r, r') = R.seq r r'
      fun >>> (w, w') = W.seq w w'
      infixr 3 >>= >> >>>



      datatype inst =
         Const of Bytestring.string
       | Dup
       | Hash160
       | Equalverify
       | Checksig

       | Unsupported


      fun instWriter i =
         (case i of
             Const s =>
                let
                   val sz = B.size s
                in
                   if sz <= 0x4b then
                      W.byte (Word8.fromInt sz)
                      >>>
                      W.bytes s
                   else
                      raise (Fail "large constant unimplemented")
                end

           | Dup => W.byte 0wx76
           | Equalverify => W.byte 0wx88
           | Hash160 => W.byte 0wxa9
           | Checksig => W.byte 0wxac

           | Unsupported =>
                raise (Fail "unsupported opcode"))


      val instReader =
         R.byte
         >>= (fn opcode =>
         (case opcode of
             0wx76 => R.return Dup
           | 0wx88 => R.return Equalverify
           | 0wxa9 => R.return Hash160
           | 0wxac => R.return Checksig
           | _ =>
                if opcode <= 0wx4b then
                   R.bytes (Word8.toInt opcode)
                   >>= (fn str =>
                   R.return (Const str)
                   )
                else
                   R.return Unsupported)
         )


      fun writer l = W.list instWriter l
      val reader = R.many instReader


      fun writeScript l = Writer.write (writer l)
      fun readScript str = Reader.readfull reader str

   end
