
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
         inputs : txin list,
         outputs : txout list,
         lockTime : Word32.word
         }



      exception InvalidTransaction

      fun writeTxin {from=(txid, n), script, sequence} =
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

      val readTxin =
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


      fun writeTxout {amount, script} =
         if amount < 0 orelse amount > maxAmount then
            raise InvalidTransaction
         else
            W.word64L (Word64.fromLargeInt amount)
            >>>
            W.bytesVar script

      val readTxout =
         R.wrap Word64.toLargeInt R.word64L
         >>= (fn amount =>
         R.bytesVar
         >>= (fn script =>
         R.return { amount=amount, script=script }
         ))
         

      fun writeTx ({inputs, outputs, lockTime}:tx) =
         W.word32L theVersion
         >>>
         W.varlist writeTxin inputs
         >>>
         W.varlist writeTxout outputs
         >>>
         W.word32L lockTime

      val readTx =
         R.word32L
         >>= (fn version =>
         if version <> theVersion then
            (* a different format, punt *)
            raise Reader.SyntaxError
         else
            R.varlist readTxin
            >>= (fn inputs =>
            R.varlist readTxout
            >>= (fn outputs =>
            R.word32L
            >>= (fn lockTime =>
            R.return { inputs=inputs, outputs=outputs, lockTime=lockTime }
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

      fun modifyForSig { inputs, outputs, lockTime } i prevScript =
         { inputs=modifyInputsForSig inputs i prevScript, outputs=outputs, lockTime=lockTime }

   end
