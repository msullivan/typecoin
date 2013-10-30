
structure Platform :> PLATFORM =
   struct

      (* Make Socket.sendVec not terminate the program when seeing an unconnected socket. *)
      val () = MLton.Signal.setHandler (Posix.Signal.pipe, MLton.Signal.Handler.ignore)
         
      fun hashWord32 x = x

   end
