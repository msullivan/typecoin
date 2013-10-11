
signature TRANSACTION =
   sig

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

      val writer : tx -> Writer.writer
      val reader : tx Reader.reader

      val writeTx : tx -> Bytestring.string
      val readTx : Bytestring.string -> tx

      (* transaction, input # to replace the script of, replacement script *)
      val modifyForSig : tx -> int -> Bytestring.string -> tx

   end
