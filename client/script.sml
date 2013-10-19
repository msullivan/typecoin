
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
       | Constn of IntInf.int
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
       | Reserved
       | Verif
       | Vernotif


      val fourByteMax =
         (case Int.maxInt of
             NONE =>
                Int64.toInt 0xffffffff
           | SOME i =>
                Int64.toInt (Int64.min (Int64.fromInt i, 0xffffffff)))

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
                   else if sz <= 0xff then
                      W.byte 0wx4c
                      >>>
                      W.byte (Word8.fromInt sz)
                      >>>
                      W.bytes s
                   else if sz <= 0xffff then
                      W.byte 0wx4d
                      >>>
                      W.word16L (Word.fromInt sz)
                      >>>
                      W.bytes s
                   else if sz <= fourByteMax then
                      W.byte 0wx4e
                      >>>
                      W.word32L (Word32.fromInt sz)
                      >>>
                      W.bytes s
                   else
                      (* Too large *)
                      raise Writer.InvalidData
                end

           | Constn i =>
                if i < 1 orelse i > 16 then
                   raise Writer.InvalidData
                else
                   W.byte (0wx50 + ConvertWord.intInfToWord8 i)

           | MinusOne => W.byte 0wx4f
           | Nop => W.byte 0wx61
           | If => W.byte 0wx63
           | Notif => W.byte 0wx64
           | Else => W.byte 0wx67
           | Endif => W.byte 0wx68
           | Verify => W.byte 0wx69
           | Return => W.byte 0wx6a
           | ToAltstack => W.byte 0wx6b
           | FromAltstack => W.byte 0wx6c
           | Ifdup => W.byte 0wx73
           | Depth => W.byte 0wx74
           | Drop => W.byte 0wx75
           | Dup => W.byte 0wx76
           | Nip => W.byte 0wx77
           | Over => W.byte 0wx78
           | Pick => W.byte 0wx79
           | Roll => W.byte 0wx7a
           | Rot => W.byte 0wx7b
           | Swap => W.byte 0wx7c
           | Tuck => W.byte 0wx7d
           | TwoDrop => W.byte 0wx6d
           | TwoDup => W.byte 0wx6e
           | ThreeDup => W.byte 0wx6f
           | TwoOver => W.byte 0wx70
           | TwoRot => W.byte 0wx71
           | TwoSwap => W.byte 0wx72
           | Cat => W.byte 0wx7e
           | Substr => W.byte 0wx7f
           | Left => W.byte 0wx80
           | Right => W.byte 0wx81
           | Size => W.byte 0wx82
           | Invert => W.byte 0wx83
           | And => W.byte 0wx84
           | Or => W.byte 0wx85
           | Xor => W.byte 0wx86
           | Equal => W.byte 0wx87
           | EqualVerify => W.byte 0wx88
           | OneAdd => W.byte 0wx8b
           | OneSub => W.byte 0wx8c
           | TwoMul => W.byte 0wx8d
           | TwoDiv => W.byte 0wx8e
           | Negate => W.byte 0wx8f
           | Abs => W.byte 0wx90
           | Not => W.byte 0wx91
           | ZeroNotEqual => W.byte 0wx92
           | Add => W.byte 0wx93
           | Sub => W.byte 0wx94
           | Mul => W.byte 0wx95
           | Div => W.byte 0wx96
           | Mod => W.byte 0wx97
           | Lshift => W.byte 0wx98
           | Rshift => W.byte 0wx99
           | BoolAnd => W.byte 0wx9a
           | BoolOr => W.byte 0wx9b
           | NumEqual => W.byte 0wx9c
           | NumEqualVerify => W.byte 0wx9d
           | NumNotEqual => W.byte 0wx9e
           | Lt => W.byte 0wx9f
           | Gt => W.byte 0wxa0
           | Leq => W.byte 0wxa1
           | Geq => W.byte 0wxa2
           | Min => W.byte 0wxa3
           | Max => W.byte 0wxa4
           | Within => W.byte 0wxa5
           | Ripemd160 => W.byte 0wxa6
           | Sha1 => W.byte 0wxa7
           | Sha256 => W.byte 0wxa8
           | Hash160 => W.byte 0wxa9
           | Hash256 => W.byte 0wxaa
           | Codeseparator => W.byte 0wxab
           | Checksig => W.byte 0wxac
           | ChecksigVerify => W.byte 0wxad
           | Checkmultisig => W.byte 0wxae
           | CheckmultisigVerify => W.byte 0wxaf
           | Reserved => W.byte 0wx50
           | Verif => W.byte 0wx65
           | Vernotif => W.byte 0wx66)


      val instReader =
         R.byte
         >>= (fn opcode =>
         if opcode <= 0wx4b then
            R.bytes (Word8.toInt opcode)
            >>= (fn str =>
            R.return (Const str)
            )
         else if opcode >= 0wx51 andalso opcode <= 0wx60 then
            R.return (Constn (ConvertWord.word8ToIntInf (opcode - 0wx50)))
         else
            (case opcode of
                0wx4c =>
                   R.wrap Word8.toInt R.byte
                   >>= (fn sz =>
                   R.bytes sz
                   >>= (fn str =>
                   R.return (Const str)
                   ))
   
              | 0wx4d =>
                   R.wrap Word.toInt R.word16L
                   >>= (fn sz =>
                   R.bytes sz
                   >>= (fn str =>
                   R.return (Const str)
                   ))
   
              | 0wx4e =>
                   R.wrap Word32.toInt R.word32L
                   >>= (fn sz =>
                   R.bytes sz
                   >>= (fn str =>
                   R.return (Const str)
                   ))
   
              | 0wx4f => R.return MinusOne
              | 0wx50 => R.return Reserved
              | 0wx61 => R.return Nop
              | 0wx62 => R.return Reserved
              | 0wx63 => R.return If
              | 0wx64 => R.return Notif
              | 0wx65 => R.return Verif
              | 0wx66 => R.return Vernotif
              | 0wx67 => R.return Else
              | 0wx68 => R.return Endif
              | 0wx69 => R.return Verify
              | 0wx6a => R.return Return
              | 0wx6b => R.return ToAltstack
              | 0wx6c => R.return FromAltstack
              | 0wx73 => R.return Ifdup
              | 0wx74 => R.return Depth
              | 0wx75 => R.return Drop
              | 0wx76 => R.return Dup
              | 0wx77 => R.return Nip
              | 0wx78 => R.return Over
              | 0wx79 => R.return Pick
              | 0wx7a => R.return Roll
              | 0wx7b => R.return Rot
              | 0wx7c => R.return Swap
              | 0wx7d => R.return Tuck
              | 0wx6d => R.return TwoDrop
              | 0wx6e => R.return TwoDup
              | 0wx6f => R.return ThreeDup
              | 0wx70 => R.return TwoOver
              | 0wx71 => R.return TwoRot
              | 0wx72 => R.return TwoSwap
              | 0wx7e => R.return Cat
              | 0wx7f => R.return Substr
              | 0wx80 => R.return Left
              | 0wx81 => R.return Right
              | 0wx82 => R.return Size
              | 0wx83 => R.return Invert
              | 0wx84 => R.return And
              | 0wx85 => R.return Or
              | 0wx86 => R.return Xor
              | 0wx87 => R.return Equal
              | 0wx88 => R.return EqualVerify
              | 0wx89 => R.return Reserved
              | 0wx8a => R.return Reserved
              | 0wx8b => R.return OneAdd 
              | 0wx8c => R.return OneSub 
              | 0wx8d => R.return TwoMul 
              | 0wx8e => R.return TwoDiv 
              | 0wx8f => R.return Negate
              | 0wx90 => R.return Abs 
              | 0wx91 => R.return Not 
              | 0wx92 => R.return ZeroNotEqual 
              | 0wx93 => R.return Add 
              | 0wx94 => R.return Sub 
              | 0wx95 => R.return Mul 
              | 0wx96 => R.return Div 
              | 0wx97 => R.return Mod 
              | 0wx98 => R.return Lshift 
              | 0wx99 => R.return Rshift 
              | 0wx9a => R.return BoolAnd 
              | 0wx9b => R.return BoolOr 
              | 0wx9c => R.return NumEqual 
              | 0wx9d => R.return NumEqualVerify 
              | 0wx9e => R.return NumNotEqual 
              | 0wx9f => R.return Lt 
              | 0wxa0 => R.return Gt 
              | 0wxa1 => R.return Leq 
              | 0wxa2 => R.return Geq 
              | 0wxa3 => R.return Min 
              | 0wxa4 => R.return Max 
              | 0wxa5 => R.return Within 
              | 0wxa6 => R.return Ripemd160 
              | 0wxa7 => R.return Sha1 
              | 0wxa8 => R.return Sha256 
              | 0wxa9 => R.return Hash160 
              | 0wxaa => R.return Hash256 
              | 0wxab => R.return Codeseparator 
              | 0wxac => R.return Checksig 
              | 0wxad => R.return ChecksigVerify 
              | 0wxae => R.return Checkmultisig 
              | 0wxaf => R.return CheckmultisigVerify
              | 0wxb0 => R.return Nop
              | 0wxb1 => R.return Nop
              | 0wxb2 => R.return Nop
              | 0wxb3 => R.return Nop
              | 0wxb4 => R.return Nop
              | 0wxb5 => R.return Nop
              | 0wxb6 => R.return Nop
              | 0wxb7 => R.return Nop
              | 0wxb8 => R.return Nop
              | 0wxb9 => R.return Nop
   
              | _ =>
                   raise R.SyntaxError)
            )


      fun writer l = W.list instWriter l
      val reader = R.many instReader


      fun writeScript l = Writer.write (writer l)
      fun readScript str = Reader.readfull reader str

   end
