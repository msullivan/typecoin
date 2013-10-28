
signature SCRIPT =
   sig

      datatype inst =
         Const of Bytestring.string
       | Constn of IntInf.int (* in 1 .. 16 *)
       | MinusOne
       | Nop
       | If
       | Notif
       | Else
       | Endif
       | Verify
       | Return
       | ToAltstack
       | FromAltstack
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
       | EqualVerify
       | OneAdd
       | OneSub
       | TwoMul
       | TwoDiv
       | Negate
       | Abs
       | Not
       | ZeroNotEqual
       | Add
       | Sub
       | Mul
       | Div
       | Mod
       | Lshift
       | Rshift
       | BoolAnd
       | BoolOr
       | NumEqual
       | NumEqualVerify
       | NumNotEqual
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
       | ChecksigVerify
       | Checkmultisig
       | CheckmultisigVerify
       | Reserved  (* same as Invalid as far as execution is concerned, but has an opcode that is sometimes treated specially *)
       | Invalid
       | Verif
       | Vernotif

      (* NB: the writers and readers are not inverses, because some instructions
         have multiple representations. *)

      val instWriter : inst -> Writer.writer
      val instReader : inst Reader.reader

      val writer : inst list -> Writer.writer
      val reader : inst list Reader.reader

      val writeScript : inst list -> Bytestring.string
      val readScript : Bytestring.string -> inst list

      val instToString : inst -> string

      val isConstant : inst -> bool

   end
