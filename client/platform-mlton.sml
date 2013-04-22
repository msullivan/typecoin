
structure Platform :> PLATFORM =
   struct

      val Socket_sameDesc = Socket.sameDesc
      fun adjustSelectTimeout x = x

   end
