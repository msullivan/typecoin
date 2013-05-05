
signature NETWORK =
   sig

      type 'mode sock = (INetSock.inet, 'mode Socket.stream) Socket.sock
      type psock = Socket.passive sock
      type asock = Socket.active sock

      val listen : int -> psock
      val accept : psock -> asock
      val connect : NetHostDB.in_addr * int -> asock
      val connectNB : NetHostDB.in_addr * int -> asock * bool

      val close : 'a sock -> unit

      val sendVec : asock * Bytesubstring.substring -> bool
      val recvVec : asock -> Bytestring.string

      val dns : string -> NetHostDB.in_addr list

   end
