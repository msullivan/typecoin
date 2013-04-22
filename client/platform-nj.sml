
structure Platform :> PLATFORM =
   struct

      val print = TextIO.print
      val Socket_connect = Socket.connect
      val Socket_close = Socket.close

   end
