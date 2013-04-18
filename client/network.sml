
structure Network =
   struct

      type sock = (INetSock.inet, Socket.active Socket.stream) Socket.sock
      
      fun listen port =
         let
            val s = INetSock.TCP.socket ()
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
            val s : sock = INetSock.TCP.socket ()
         in
            Socket.connect (s, saddr);
            s
         end
      
      fun send (s, str) =
         Socket.sendVec (s, Bytesubstring.full str)
      
      fun recv s =
         Socket.recvVec (s, 1024)
      
      fun recvNB s =
         Socket.recvVecNB (s, 1024)
      
      fun dns str =
         map NetHostDB.toString (NetHostDB.addrs (valOf (NetHostDB.getByName str)))

   end

