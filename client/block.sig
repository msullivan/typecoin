
signature BLOCK =
   sig

      type header =
         {
         version : int,
         previous : Bytestring.string,
         root : Bytestring.string,
         timestamp : Word32.word,
         bits : Word32.word,  (* encoded difficulty *)
         nonce : Word32.word
         }

      type block = header * Transaction.tx list

      exception InvalidBlock

      val headerWriter : header -> Writer.writer
      val headerReader : header Reader.reader

      val writer : block -> Writer.writer
      val reader : block Reader.reader

      val readBlock : Bytestring.string -> block

      (* Argument function takes:
         - transaction index
         - transaction offset (bytes into the block)
         - the transaction
         - the serialized transaction
         - accumulator
      *)
      val traverseBlock : (int * int * Transaction.tx * Bytesubstring.substring * 'a -> 'a) -> 'a -> Bytestring.string -> 'a

   end
