
signature WRITER =
   sig

      exception InvalidData

      type writer = Bytestring.string Output.output

      val null : writer
      val seq : writer -> writer -> writer
      val seql : writer list -> writer
      val repeat : int -> writer -> writer
      val list : ('a -> writer) -> 'a list -> writer

      val byte : Word8.word -> writer
      val word16B : Word.word -> writer
      val word16L : Word.word -> writer
      val word32L : Word32.word -> writer
      val word64L : Word64.word -> writer
      val bytes : Bytestring.string -> writer
      val bytesS : Bytesubstring.substring -> writer
      val varint : int -> writer

      val bytesVar : Bytestring.string -> writer
      val bytesPad : int -> Bytestring.string -> writer
      val bytesFixed : int -> Bytestring.string -> writer
      val varlist : ('a -> writer) -> 'a list -> writer

      val write : writer -> Bytestring.string
      val writeOutstream : BinIO.outstream -> writer -> unit

   end
