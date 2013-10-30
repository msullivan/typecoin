
signature UNITYPED =
   sig

      datatype unityped =
         Nil
       | True
       | Byte of Word8.word
       | Int of int
       | Integer of IntInf.int  (* unsigned for now *)
       | String of string
       | Bytestring of Bytestring.string
       | Cons of unityped * unityped
       | Exception of unityped
       | Method

      val writer : unityped -> Writer.writer
      val reader : unityped Reader.reader

   end
