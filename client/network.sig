
signature NETWORK =
   sig

      type addr = NetHostDB.in_addr
      type 'mode sock = (INetSock.inet, 'mode Socket.stream) Socket.sock
      type psock = Socket.passive sock
      type asock = Socket.active sock

      val listen : int -> psock
      val accept : psock -> asock * addr * int
      val connect : addr * int -> asock
      val connectNB : addr * int -> asock * bool

      val close : 'a sock -> unit

      val sendVec : asock * Bytesubstring.substring -> bool
      val recvVec : asock -> Bytestring.string
      
      val dns : string -> addr list
      val self : unit -> addr list

   end
