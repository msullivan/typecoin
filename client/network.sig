
signature NETWORK =
   sig

      type 'mode sock = (INetSock.inet, 'mode Socket.stream) Socket.sock
      type psock = Socket.passive sock
      type asock = Socket.active sock
      type addr = NetHostDB.in_addr

      val listen : int -> psock
      val accept : psock -> asock
      val connect : addr * int -> asock

      val close : 'a sock -> unit
      val tryClose : 'a sock -> unit

      val sendVec : asock * Bytesubstring.substring -> bool
      val recvVec : asock -> Bytestring.string

      val dns : string -> addr list
      val implodeAddr : Word8.word list -> addr
      val explodeAddr : addr -> Word8.word list

   end
