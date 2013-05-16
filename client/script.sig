
signature SCRIPT =
   sig

      datatype inst =
         Const of Bytestring.string
       | Constn of IntInf.int
       | Nop
       | If
       | Notif
       | Else
       | Endif
       | Verify
       | Return
       | Toaltstack
       | Fromaltstack
       | Ifdup
       | Depth
       | Drop
       | Dup
       | Nip
       | Over
       | Pick
       | Roll
       | Rot
       | Swap
       | Tuck
       | TwoDrop
       | TwoDup
       | ThreeDup
       | TwoOver
       | TwoRot
       | TwoSwap
       | Cat
       | Substr
       | Left
       | Right
       | Size
       | Invert
       | And
       | Or
       | Xor
       | Equal
       | Equalverify
       | OneAdd
       | OneSub
       | TwoMul
       | TwoDiv
       | Negate
       | Abs
       | Not
       | ZeroNotequal
       | Add
       | Sub
       | Mul
       | Div
       | Mod
       | Lshift
       | Rshift
       | Booland
       | Boolor
       | Numequal
       | Numequalverify
       | Numnotequal
       | Lt
       | Gt
       | Leq
       | Geq
       | Min
       | Max
       | Within
       | Ripemd160
       | Sha1
       | Sha256
       | Hash160
       | Hash256
       | Codeseparator
       | Checksig
       | Checksigverify
       | Checkmultisig
       | Checkmultisigverify

       | Unsupported

      val writer : inst list -> Writer.writer
      val reader : inst list Reader.reader

      val writeScript : inst list -> Bytestring.string
      val readScript : Bytestring.string -> inst list

   end
