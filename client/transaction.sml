
structure Transaction :> TRANSACTION =
   struct

      structure B = Bytestring
      structure W = Writer

      (* check architecture *)
      val () =
         let val w = 0wxffffffffffffffff : Word64.word
         in
            if Word64.fromLargeInt (Word64.toLargeInt w) = w then
               ()
            else
               raise (Fail "LargeInt.int is smaller than 64 bits.")
         end


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

      val version = ConvertWord.word32ToBytesL 0w1
      val maxAmount = Word64.toLargeInt 0wxffffffffffffffff
      val power32 = Word64.toLargeInt 0wx100000000


      fun writeTxin {from=(txid, n), script, sequence} =
         if B.size txid <> 32 then
            raise InvalidTransaction
         else
            W.seql
            [ W.bytes (B.rev txid),
              W.word32L n,
              W.bytesVar script,
              W.word32L sequence ]


      fun writeTxout {amount, script} =
         if amount < 0 orelse amount > maxAmount then
            raise InvalidTransaction
         else
            W.seq (W.word64L (Word64.fromLargeInt amount),
                   W.bytesVar script)
         

      fun write ({inputs, outputs, lockTime}:tx) =
         W.seql
         [
         W.bytes version,
         W.bytes (W.sizemark (length inputs)),
         W.seql (map writeTxin inputs),
         W.bytes (W.sizemark (length outputs)),
         W.seql (map writeTxout outputs),
         W.word32L lockTime
         ]

      fun modifyForSig inputs i prevScript =
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

      fun writeForSig ({inputs, outputs, lockTime}:tx) i prevScript suffix =
         W.seq (write {inputs=modifyForSig inputs i prevScript, outputs=outputs, lockTime=lockTime},
                W.bytes suffix)

   end
