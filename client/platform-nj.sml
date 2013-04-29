
structure Platform :> PLATFORM =
   struct

      val Socket_sameDesc = Socket.sameDesc
      fun adjustSelectTimeout x = x

      fun hashWord32 w =
         MJHash.hashInc (ConvertWord.word32ToWord w) (ConvertWord.word32ToWord (Word32.>> (w, 0w1)))

   end
