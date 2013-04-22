
structure Network (* :> NETWORK *) =
   struct

      type 'a sock = (INetSock.inet, 'a Socket.stream) Socket.sock
      type psock = Socket.passive sock
      type asock = Socket.active sock
      type addr = NetHostDB.in_addr


      (* constants *)
      val bufsize = 1024


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
            print "Connection from ";
            print (NetHostDB.toString (#1 (INetSock.fromAddr a)));
            print "\n";
            s'
         end
      
      fun connect (addr, port) =
         let
            val saddr = INetSock.toAddr (addr, port)
            val s : asock = INetSock.TCP.socket ()
         in
            print "Connecting to ";
            print (NetHostDB.toString addr);
            print "\n";
            Socket.connect (s, saddr);
            print "Connected\n";
            s
         end

      val close = Socket.close
      
      fun tryClose sock = (close sock handle OS.SysErr _ => ())

      fun sendVec (sock, v) =
         let
            val n = Socket.sendVec (sock, v)
         in
            Bytesubstring.size v = n
         end

(*
      val log : (asock * Bytestring.string) list ref = ref []
*)

      fun recvVec s =
         let
            val v = Socket.recvVec (s, bufsize)
         in
            print ".";
(*
            log := (s, v) :: !log;
*)
            v
         end




      fun dns str =
         NetHostDB.addrs (valOf (NetHostDB.getByName str))

      fun implodeAddr l =
         let
            val str = String.concatWith "." (map (Int.toString o Word8.toInt) l)
         in
            (case NetHostDB.getByName str of
                NONE =>
                   raise (Fail "bad IP address")
              | SOME entry =>
                   NetHostDB.addr entry)
         end

      fun explodeAddr addr =
         let
            val l = String.fields (fn ch => ch = #".") (NetHostDB.toString addr)
         in
            if length l = 4 then
               map (Word8.fromInt o valOf o Int.fromString) l
            else
               raise (Fail "bad address string")
         end

   end
