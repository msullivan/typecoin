
structure Unityped :> UNITYPED =
   struct

      structure B = Bytestring
      structure R = Reader
      structure W = Writer

      fun >>= (r, f) = R.bind r f
      fun >> (r, r') = R.seq r r'
      fun >>> (w, w') = W.seq w w'
      infixr 3 >>= >> >>>


      datatype unityped =
         Nil
       | True
       | Byte of Word8.word
       | Int of int
       | Integer of IntInf.int
       | String of string
       | Bytestring of Bytestring.string
       | Cons of unityped * unityped
       | Exception of unityped
       | Method


      fun writer resp =
         (case resp of
             Nil =>
                W.byte 0w1
           | True =>
                W.byte 0w2
           | Byte b =>
                W.byte 0w3
                >>>
                W.byte b
           | Int i =>
                (* restrict ourselves to 31-bit ints, for compatibility *)
                if i < ~1073741824 orelse i > 0x3fffffff then
                   raise W.InvalidData
                else
                   W.byte 0w4
                   >>>
                   W.word32B (Word32.fromInt i)
           | Integer i =>
                if i < 0 then
                   raise W.InvalidData
                else
                   W.byte 0w5
                   >>>
                   W.bytesVar (ConvertIntInf.toBytesB i)
           | String str =>
                W.byte 0w6
                >>>
                W.bytesVar (B.fromString str)
           | Bytestring str =>
                W.byte 0w7
                >>>
                W.bytesVar str
           | Cons (resp1, resp2) =>
                W.byte 0w8
                >>>
                writer resp1
                >>>
                writer resp2
           | Exception resp =>
                W.byte 0w9
                >>>
                writer resp
           | Method =>
                W.byte 0w10)

      fun reader' () =
         R.byte
         >>= (fn branch =>
         (case branch of
             0w1 =>
                R.return Nil
           | 0w2 =>
                R.return True
           | 0w3 =>
                R.wrap Byte R.byte
           | 0w4 =>
                R.word32B
                >>= (fn w =>
                R.return (Int (Word32.toInt w) handle Overflow => raise R.SyntaxError)
                )
           | 0w5 =>
                R.wrap (fn str => Integer (ConvertIntInf.fromBytesB str)) R.bytesVar
           | 0w6 =>
                R.wrap (fn str => String (B.toString str)) R.bytesVar
           | 0w7 =>
                R.wrap Bytestring R.bytesVar
           | 0w8 =>
                reader' ()
                >>= (fn x =>
                reader' ()
                >>= (fn y =>
                R.return (Cons (x, y))
                ))
           | 0w9 =>
                R.wrap Exception (reader' ())
           | 0w10 =>
                R.return Method
           | _ =>
                raise R.SyntaxError)
         )

      val reader = reader' ()

   end
