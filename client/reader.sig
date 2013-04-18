
signature READER =
   sig

      include PARSING
              where type token = Word8.word
              where type Streamable.t = Bytesubstring.substring

      type 'a reader = 'a parser

      val sizemark : int reader
      val byte : Word8.word reader
      val word16B : Word.word reader
      val word32L : Word32.word reader
      val word64L : Word64.word reader
      val bytes : int -> Bytestring.string reader

      val bytesVar : Bytestring.string reader

      val read : 'a reader -> Bytesubstring.substring -> 'a * Bytesubstring.substring

   end
