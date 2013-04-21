
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

      val readBlock : block Reader.reader

   end
