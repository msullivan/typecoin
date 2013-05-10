
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

      (* precomputed values *)
      val power32 = Word64.toLargeInt 0wx100000000
      val maxAmount = Word64.toLargeInt 0wxffffffffffffffff



      type coord = Bytestring.string * Word32.word

      type txin =
         {
         from : coord,
         script : Bytestring.string,
         sequence : Word32.word
         }
         
      type txout =
         {
         amount : LargeInt.int,
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
            W.word32L n
            >>>
            W.bytesVar script
            >>>
            W.word32L sequence

      val txinReader =
         R.bytes 32
         >>= (fn txid =>
         R.word32L
         >>= (fn n =>
         R.bytesVar
         >>= (fn script =>
         R.word32L
         >>= (fn sequence =>
         R.return { from=(txid, n), script=script, sequence=sequence }
         ))))


      fun txoutWriter {amount, script} =
         if amount < 0 orelse amount > maxAmount then
            raise InvalidTransaction
         else
            W.word64L (Word64.fromLargeInt amount)
            >>>
            W.bytesVar script

      val txoutReader =
         R.wrap Word64.toLargeInt R.word64L
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



      fun modifyInputsForSig inputs i prevScript =
         let
            fun loop j l =
               (case l of
                   [] =>
                      if i > j then
                         raise InvalidTransaction
                      else
                         []
                 | {from, script=_, sequence} :: rest =>
                      if i = j then
                         {from=from, script=prevScript, sequence=sequence}
                         :: loop (j+1) rest
                      else
                         {from=from, script=B.null, sequence=sequence}
                         :: loop (j+1) rest)
         in
            if i < 0 then
               raise InvalidTransaction
            else
               loop 0 inputs
         end

      fun modifyForSig { version, inputs, outputs, lockTime } i prevScript =
         { version=version, inputs=modifyInputsForSig inputs i prevScript, outputs=outputs, lockTime=lockTime }

   end
