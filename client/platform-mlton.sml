
structure Platform :> PLATFORM =
   struct

      val () = MLton.Signal.setHandler (Posix.Signal.pipe, MLton.Signal.Handler.ignore)
         
      fun hashWord32 x = x

   end
