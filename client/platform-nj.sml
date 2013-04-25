
structure Platform :> PLATFORM =
   struct

      (* Work around broken Socket.sameDesc. *)
      fun getFd sd = Unsafe.Object.toInt (List.nth (Unsafe.Object.toTuple (Unsafe.Object.toObject sd), 1))
      fun Socket_sameDesc (sd, sd') = getFd sd = getFd sd'

      (* On Windows, NJ waits twice the indicated timeout. *)
      fun adjustSelectTimeout t = Time.fromMilliseconds (Time.toMilliseconds t div 2)

      fun hashWord32 w =
         MJHash.hashInc (ConvertWord.word32ToWord w) (ConvertWord.word32ToWord (Word32.>> (w, 0w1)))

   end
