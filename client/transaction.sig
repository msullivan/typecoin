
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
         inputs : txin list,
         outputs : txout list,
         lockTime : Word32.word
         }

      exception InvalidTransaction

      val writeTxin : txin -> Writer.writer
      val writeTxout : txout -> Writer.writer
      val write : tx -> Writer.writer
      val writeForSig : tx -> int -> Bytestring.string -> Bytestring.string -> Writer.writer

   end
