
signature PLATFORM =
   sig

      val Socket_sameDesc : Socket.sock_desc * Socket.sock_desc -> bool
      val adjustSelectTimeout : Time.time -> Time.time

   end
