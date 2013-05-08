
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

      val writeBlock : block -> Writer.writer
      val writeBlockHeader : block -> Writer.writer
      val readBlock : block Reader.reader

      (* hash the first 80 bytes of the string *)
      val hashBlockHeader : Bytestring.string -> Bytestring.string

      (* merkle n l: if  n = |l| and n > 0  then  return the merkle root of l *)
      val merkleRoot : int -> Transaction.tx list -> Bytestring.string

   end
