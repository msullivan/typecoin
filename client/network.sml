
structure Network :> NETWORK =
   struct

      type addr = NetHostDB.in_addr
      type 'a sock = (INetSock.inet, 'a Socket.stream) Socket.sock
      type psock = Socket.passive sock
      type asock = Socket.active sock


      (* constants *)
      val bufsize = 4096

      
      exception NetworkException of string * exn


      fun listen port =
         let
            val s : psock = INetSock.TCP.socket ()
            val () = Socket.bind (s, INetSock.any port)
            val () = Socket.listen (s, 5)
         in
            s
         end handle exn => raise (NetworkException ("listen", exn))


      fun accept s =
         let
            val (s', a) = Socket.accept s
            val (addr, port) = INetSock.fromAddr a
         in
            Log.long (fn () => "Connection from " ^ NetHostDB.toString addr);
            (s', addr, port)
         end handle exn => raise (NetworkException ("accept", exn))


      fun connect (addr, port) =
         let
            val saddr = INetSock.toAddr (addr, port)
            val s : asock = INetSock.TCP.socket ()
         in
            Log.long (fn () => "Connecting to " ^ NetHostDB.toString addr);
            Socket.connect (s, saddr);
            s
         end handle exn => raise (NetworkException ("connect", exn))


      fun connectNB (addr, port) =
         let
            val saddr = INetSock.toAddr (addr, port)
            val s : asock = INetSock.TCP.socket ()
         in
            Log.long (fn () => "Connecting to " ^ NetHostDB.toString addr);
            (s, Socket.connectNB (s, saddr))
         end handle exn => raise (NetworkException ("connectNB", exn))


      fun close sock = Socket.close sock handle OS.SysErr _ => ()


      fun sendVec (sock, v) =
         let
            (* MLton: note signal handling in platform-mlton.sml. *)
            val n = Socket.sendVec (sock, v)
         in
            Bytesubstring.size v = n
         end
         handle OS.SysErr (err, _) =>
            (
            Log.long (fn () => "Send error: " ^ err);
            false
            )
                                       

      fun recvVec s =
         Socket.recvVec (s, bufsize)
         handle OS.SysErr (err, _) =>
            (
            Log.long (fn () => "Receive error: " ^ err);
            Bytestring.null
            )


      fun dns str =
         getOpt (Option.map NetHostDB.addrs (NetHostDB.getByName str), [])

      fun self () = dns (NetHostDB.getHostName ())

   end
