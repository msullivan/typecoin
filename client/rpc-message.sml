
structure RpcMessage :> RPC_MESSAGE =
   struct

      structure B = Bytestring
      structure R = Reader
      structure W = Writer

      fun >>= (r, f) = R.bind r f
      fun >> (r, r') = R.seq r r'
      fun >>> (w, w') = W.seq w w'
      infixr 3 >>= >> >>>


      datatype request =
         CloseChannel
       | ShutdownServer
       | BlockMember of Bytestring.string
       | LookupBlock of Bytestring.string
       | BlockPrimary of Bytestring.string
       | LastBlock
       | TotalDifficulty
       | BlockByNumber of int
       | LookupTx of Bytestring.string
       | TxByNumber of int * int
       | Inject of Transaction.tx

      datatype response =
         True
       | False
       | Int of int
       | Integer of IntInf.int
       | String of Bytestring.string
       | Cons of response * response
       | Exception of response
       | Syntax
       | Transaction of Transaction.tx


      fun intWriter i =
         (* restrict ourselves to 31-bit ints, for compatibility *)
         if i < ~1073741824 orelse i > 0x3fffffff then
            raise W.InvalidData
         else
            W.word32B (Word32.fromInt i)

      val intReader =
         R.word32B
         >>= (fn w =>
         R.return (Word32.toInt w handle Overflow => raise R.SyntaxError)
         )

      fun integerWriter i =
         W.bytesVar (ConvertIntInf.toBytesB i)

      val integerReader =
         R.bytesVar >>= (fn str => R.return (ConvertIntInf.fromBytesB str))


      fun requestWriter req =
         (case req of
             CloseChannel =>
                W.byte 0w1

           | ShutdownServer =>
                W.byte 0w2

           | BlockMember str =>
                W.byte 0w3
                >>>
                W.bytesVar str

           | LookupBlock str =>
                W.byte 0w4
                >>>
                W.bytesVar str

           | BlockPrimary str =>
                W.byte 0w5
                >>>
                W.bytesVar str

           | LastBlock =>
                W.byte 0w6

           | TotalDifficulty =>
                W.byte 0w7

           | BlockByNumber i =>
                W.byte 0w8
                >>>
                intWriter i

           | LookupTx str =>
                W.byte 0w9
                >>>
                W.bytesVar str

           | TxByNumber (i, j) =>
                W.byte 0w10
                >>>
                intWriter i
                >>>
                intWriter j

           | Inject tx =>
                W.byte 0w11
                >>>
                Transaction.writer tx)


      val requestReader =
         R.byte
         >>= (fn branch =>
         (case branch of
             0w1 =>
                R.return CloseChannel
           | 0w2 =>
                R.return ShutdownServer
           | 0w3 =>
                R.wrap BlockMember R.bytesVar
           | 0w4 =>
                R.wrap LookupBlock R.bytesVar
           | 0w5 =>
                R.wrap BlockPrimary R.bytesVar
           | 0w6 =>
                R.return LastBlock
           | 0w7 =>
                R.return TotalDifficulty
           | 0w8 =>
                R.wrap BlockByNumber intReader
           | 0w9 =>
                R.wrap LookupTx R.bytesVar
           | 0w10 =>
                intReader
                >>= (fn i =>
                intReader
                >>= (fn j =>
                R.return (TxByNumber (i, j))
                ))
           | 0w11 =>
                R.wrap Inject Transaction.reader
           | _ =>
                raise R.SyntaxError)
         )


      fun responseWriter resp =
         (case resp of
             False =>
                W.byte 0w1
           | True =>
                W.byte 0w2
           | Int i =>
                W.byte 0w3
                >>>
                intWriter i
           | Integer i =>
                if i < 0 then
                   raise W.InvalidData
                else
                   W.byte 0w4
                   >>>
                   integerWriter i
           | String str =>
                W.byte 0w5
                >>>
                W.bytesVar str
           | Cons (resp1, resp2) =>
                W.byte 0w6
                >>>
                responseWriter resp1
                >>>
                responseWriter resp2
           | Exception resp =>
                W.byte 0w7
                >>>
                responseWriter resp
           | Syntax =>
                W.byte 0w8
           | Transaction tx =>
                W.byte 0w9
                >>>
                Transaction.writer tx)

      fun responseReader' () =
         R.byte
         >>= (fn branch =>
         (case branch of
             0w1 =>
                R.return False
           | 0w2 =>
                R.return True
           | 0w3 =>
                R.wrap Int intReader
           | 0w4 =>
                R.wrap Integer integerReader
           | 0w5 =>
                R.wrap String R.bytesVar
           | 0w6 =>
                responseReader' ()
                >>= (fn x =>
                responseReader' ()
                >>= (fn y =>
                R.return (Cons (x, y))
                ))
           | 0w7 =>
                R.wrap Exception (responseReader' ())
           | 0w8 =>
                R.return Syntax
           | 0w9 =>
                R.wrap Transaction Transaction.reader
           | _ =>
                raise R.SyntaxError)
         )

      val responseReader = responseReader' ()
      
   end