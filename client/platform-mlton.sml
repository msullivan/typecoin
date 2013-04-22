
structure Platform :> PLATFORM =
   struct

      val atomically = MLton.Thread.atomically

      fun print str = atomically (fn () => TextIO.print str)
      fun Socket_connect x = atomically (fn () => Socket.connect x)
      fun Socket_close sock = atomically (fn () => Socket.close sock)

   end
