
structure RpcMessage :> RPC_MESSAGE =
   struct

      structure B = Bytestring
      structure R = Reader
      structure W = Writer

      fun >>= (r, f) = R.bind r f
      fun >> (r, r') = R.seq r r'
      fun >>> (w, w') = W.seq w w'
      infixr 3 >>= >> >>>


      (* No logical order to these, just the order in which they were added. *)

      datatype request =
         Inject of Transaction.tx
       | LookupTx of Bytestring.string
       | CloseChannel
       | LastBlock
       | PositionByNumber of int

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
             Inject tx =>
                W.byte 0w1
                >>>
                Transaction.writer tx

           | LookupTx str =>
                W.byte 0w2
                >>>
                W.bytesVar str

           | CloseChannel =>
                W.byte 0w3

           | LastBlock =>
                W.byte 0w4

           | PositionByNumber i =>
                W.byte 0w5
                >>>
                intWriter i)

      val requestReader =
         R.byte
         >>= (fn branch =>
         (case branch of
             0w1 =>
                R.wrap Inject Transaction.reader
           | 0w2 =>
                R.wrap LookupTx R.bytesVar
           | 0w3 =>
                R.return CloseChannel
           | 0w4 =>
                R.return LastBlock
           | 0w5 =>
                R.wrap PositionByNumber intReader
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