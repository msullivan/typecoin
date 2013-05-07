
signature TRANSACTION =
   sig

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

      val mkTx :
         { inputs : txin list, outputs : txout list, lockTime : Word32.word } -> tx

      val writeTx : tx -> Writer.writer
      val readTx : tx Reader.reader

      (* transaction, input # to replace the script of, replacement script *)
      val modifyForSig : tx -> int -> Bytestring.string -> tx

   end
