
signature READER =
   sig

      include PARSING
              where type token = Word8.word
              where type Streamable.t = Bytesubstring.substring

      type 'a reader = 'a parser

      val varint : int reader
      val byte : Word8.word reader
      val word16B : Word.word reader
      val word16L : Word.word reader
      val word32L : Word32.word reader
      val word64L : Word64.word reader
      val bytes : int -> Bytestring.string reader
      val bytesS : int -> Bytesubstring.substring reader
      val all : Bytesubstring.substring reader

      val bytesVar : Bytestring.string reader
      val varlist : 'a reader -> 'a list reader

      val read : 'a reader -> Bytestring.string -> 'a * Bytesubstring.substring
      val readS : 'a reader -> Bytesubstring.substring -> 'a * Bytesubstring.substring
      val readfull : 'a reader -> Bytestring.string -> 'a
      val readfullS : 'a reader -> Bytesubstring.substring -> 'a

   end
