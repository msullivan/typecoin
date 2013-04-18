
signature WRITER =
   sig

      exception InvalidData

      type writer

      val seq : writer * writer -> writer
      val seql : writer list -> writer
      val repeat : int -> writer -> writer

      val byte : Word8.word -> writer
      val word16B : Word.word -> writer
      val word32L : Word32.word -> writer
      val word64L : Word64.word -> writer
      val bytes : Bytestring.string -> writer

      val bytesVar : Bytestring.string -> writer
      val bytesPad : int -> Bytestring.string -> writer

      val write : writer -> Bytestring.string


      val sizemark : int -> Bytestring.string

   end
