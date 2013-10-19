
structure Transaction :> TRANSACTION =
   struct

      (* check architecture *)
      val () =
         let val w = 0wxffffffffffffffff : Word64.word
         in
            if Word64.fromLargeInt (Word64.toLargeInt w) = w then
               ()
            else
               raise (Fail "LargeInt.int is smaller than 64 bits.")
         end



      structure B = Bytestring
      structure W = Writer
      structure R = Reader

      fun >>= (r, f) = R.bind r f
      fun >> (r, r') = R.seq r r'
      fun >>> (w, w') = W.seq w w'
      infixr 3 >>= >> >>>


      (* constants *)
      val theVersion : Word32.word = 0w1



      fun sword32ToInt w =
         if Word32.andb (w, 0wx80000000) = 0w0 then
            Word32.toInt w
         else
            ~ (Word32.toInt (Word32.~ w))

      fun intToSword32 i =
         if i >= 0 then
            Word32.fromInt i
         else
            Word32.~ (Word32.fromInt (~ i))



      type hash = Bytestring.string
      type coord = hash * int

      type txin =
         {
         from : coord,
         script : Bytestring.string,
         sequence : Word32.word
         }
         
      type txout =
         {
         amount : Word64.word,
         script : Bytestring.string
         }

      type tx =
         {
         version : Word32.word,
         inputs : txin list,
         outputs : txout list,
         lockTime : Word32.word
         }



      exception InvalidTransaction


      fun mkTx { inputs, outputs, lockTime } =
         { version=theVersion, inputs=inputs, outputs=outputs, lockTime=lockTime }


      fun txinWriter {from=(txid, n), script, sequence} =
         if B.size txid <> 32 then
            raise InvalidTransaction
         else
            W.bytes txid
            >>>
            W.word32L (intToSword32 n)
            >>>
            W.bytesVar script
            >>>
            W.word32L sequence

      val txinReader =
         R.bytes 32
         >>= (fn txid =>
         R.wrap sword32ToInt R.word32L
         >>= (fn n =>
         R.bytesVar
         >>= (fn script =>
         R.word32L
         >>= (fn sequence =>
         R.return { from=(txid, n), script=script, sequence=sequence }
         ))))


      fun txoutWriter {amount, script} =
         W.word64L amount
         >>>
         W.bytesVar script

      val txoutReader =
         R.word64L
         >>= (fn amount =>
         R.bytesVar
         >>= (fn script =>
         R.return { amount=amount, script=script }
         ))
         

      fun writer ({version, inputs, outputs, lockTime}:tx) =
         W.word32L version
         >>>
         W.varlist txinWriter inputs
         >>>
         W.varlist txoutWriter outputs
         >>>
         W.word32L lockTime

      val reader =
         R.word32L
         >>= (fn version =>
         R.varlist txinReader
         >>= (fn inputs =>
         R.varlist txoutReader
         >>= (fn outputs =>
         R.word32L
         >>= (fn lockTime =>
         R.return { version=version, inputs=inputs, outputs=outputs, lockTime=lockTime }
         ))))


      fun writeTx tx = Writer.write (writer tx)

      fun readTx str = Reader.readfull reader str

   end
