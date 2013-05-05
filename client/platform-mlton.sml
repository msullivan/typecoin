
structure Platform :> PLATFORM =
   struct

      val Socket_sameDesc = Socket.sameDesc
      fun adjustSelectTimeout x = x
      fun hashWord32 x = x

   end
