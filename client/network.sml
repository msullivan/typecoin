
structure Network :> NETWORK =
   struct

      type 'a sock = (INetSock.inet, 'a Socket.stream) Socket.sock
      type psock = Socket.passive sock
      type asock = Socket.active sock
      type addr = NetHostDB.in_addr


      (* constants *)
      val bufsize = 4096


      fun listen port =
         let
            val s : psock = INetSock.TCP.socket ()
            val () = Socket.bind (s, INetSock.any port)
            val () = Socket.listen (s, 5)
         in
            s
         end
      
      fun accept s =
         let
            val (s', a) = Socket.accept s
         in
            Log.long (fn () => "Connection from " ^ NetHostDB.toString (#1 (INetSock.fromAddr a)));
            s'
         end
      
      fun connect (addr, port) =
         let
            val saddr = INetSock.toAddr (addr, port)
            val s : asock = INetSock.TCP.socket ()
         in
            Log.long (fn () => "Connecting to " ^ NetHostDB.toString addr);
            Socket.connect (s, saddr);
            s
         end

      fun connectNB (addr, port) =
         let
            val saddr = INetSock.toAddr (addr, port)
            val s : asock = INetSock.TCP.socket ()
         in
            Log.long (fn () => "Connecting to " ^ NetHostDB.toString addr);
            (s, Socket.connectNB (s, saddr))
         end

      val close = Socket.close
      
      fun tryClose sock = (close sock handle OS.SysErr _ => ())

      fun sendVec (sock, v) =
         let
            val n = Socket.sendVec (sock, v)
         in
            Bytesubstring.size v = n
         end handle OS.SysErr (err, _) =>
            (
            Log.long (fn () => "Send error: " ^ err);
            false
            )
                                       

      fun recvVec s =
         let
            val v = Socket.recvVec (s, bufsize)
         in
            Log.short ".";
            v
         end handle OS.SysErr (err, _) =>
            (
            Log.long (fn () => "Receive error: " ^ err);
            Bytestring.null
            )


      fun dns str =
         getOpt (Option.map NetHostDB.addrs (NetHostDB.getByName str), [])

   end
