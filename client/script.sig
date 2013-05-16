
signature SCRIPT =
   sig

      datatype inst =
         Const of Bytestring.string
       | Dup
       | Hash160
       | Equalverify
       | Checksig

       | Unsupported

      val writer : inst list -> Writer.writer
      val reader : inst list Reader.reader

      val writeScript : inst list -> Bytestring.string
      val readScript : Bytestring.string -> inst list

   end
