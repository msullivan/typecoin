
structure Platform :> PLATFORM =
   struct

      val Socket_sameDesc = Socket.sameDesc
      fun adjustSelectTimeout x = x
      val BinIO_openAppend = BinIO.openAppend
      fun hashWord32 x = x

   end
