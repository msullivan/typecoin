
structure Network :> NETWORK =
   struct

      type 'a sock = (INetSock.inet, 'a Socket.stream) Socket.sock
      type psock = Socket.passive sock
      type asock = Socket.active sock
      type addr = NetHostDB.in_addr


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
         in
            Log.long (fn () => "Connection from " ^ NetHostDB.toString (#1 (INetSock.fromAddr a)));
            s'
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
(*
            Log.short ".";
*)
            v
         end handle OS.SysErr (err, _) =>
            (
            Log.long (fn () => "Receive error: " ^ err);
            Bytestring.null
            )


      fun dns str =
         getOpt (Option.map NetHostDB.addrs (NetHostDB.getByName str), [])

   end
