
signature PLATFORM =
   sig

      val Socket_sameDesc : Socket.sock_desc * Socket.sock_desc -> bool
      val adjustSelectTimeout : Time.time -> Time.time
      val BinIO_openAppend : string -> BinIO.outstream
      val hashWord32 : Word32.word -> Word.word

   end
