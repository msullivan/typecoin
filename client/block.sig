
signature BLOCK =
   sig

      type block =
         {
         version : int,
         previous : Bytestring.string,
         root : Bytestring.string,
         timestamp : Word32.word,
         bits : Word32.word,  (* encoded difficulty *)
         nonce : Word32.word,
         count : int,
         transactions : Transaction.tx list
         }

      exception InvalidBlock

      val writer : block -> Writer.writer
      val headerWriter : block -> Writer.writer
      val reader : block Reader.reader

      val readBlock : Bytesubstring.substring -> block

   end
