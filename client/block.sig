
signature BLOCK =
   sig

      type block =
         {
         version : int,
         previous : Bytestring.string,
         root : Bytestring.string,
         timestamp : Word32.word,
         difficulty : Word32.word,
         nonce : Word32.word,
         count : int,
         transactions : Transaction.tx list
         }

      exception InvalidBlock

      val writeBlock : block -> Writer.writer
      val writeBlockHeader : block -> Writer.writer
      val readBlock : block Reader.reader

      val hashBlockString : Bytestring.string -> Bytestring.string

   end
