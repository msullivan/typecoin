
signature MESSAGE =
   sig

      type word8 = Word8.word
      type word32 = Word32.word
      type word64 = Word64.word

      exception InvalidMessage

      type netaddr =
         {
         services : word64,
         address : Address.addr,
         port : int
         }

      type version =
         {
         version : int,
         services : word64,  (* bitfield *)
         timestamp : LargeInt.int,
         self : netaddr,
         remote : netaddr,
         nonce : word64,
         agent : string,
         lastBlock : int,
         relay : bool
         }

      val serviceNetwork : word64

      type tnetaddr = LargeInt.int * netaddr

      type getblocks =
         {
         version : int,
         hashes : Bytestring.string list,
         lastDesired : Bytestring.string option
         }
         
      datatype objtp = ERR | TX | BLOCK

      type inv = objtp * Bytestring.string  (* length = 32 *)

      type rawalert = Bytestring.string * Bytestring.string

      datatype message =
         Version of version
       | Verack
       | Addr of tnetaddr list
       | Inv of inv list
       | Getdata of inv list
       | Notfound of inv list
       | Getblocks of getblocks
       | Tx of Bytestring.string
       | Block of Bytestring.string
       | Getaddr
       | Ping of Word64.word
       | Pong of Word64.word

         (* incoming only *)
       | Alert of rawalert
       | OldVersion of int
       | Unsupported of Bytestring.string
       | Illformed of Bytestring.string



      val mkNetaddr : Address.addr -> netaddr

      val mkVersion :
         { self : netaddr, remote : netaddr, nonce : word64, lastBlock : int } -> version

      val mkGetblocks :
         { hashes : Bytestring.string list, lastDesired : Bytestring.string option } -> getblocks



      val writeMessage : message -> Bytestring.string

      (* precondition: magic number and message size are correct *)
      val readMessage : Bytesubstring.substring -> message

      val parseAlert : { version : int,
                         relayUntil : LargeInt.int,
                         expiration : LargeInt.int,
                         id : Word32.word,
                         cancel : Word32.word,
                         setCancel : Word32.word list,
                         minVer : Word32.word,
                         maxVer : Word32.word,
                         setSubVer : string list,
                         priority : Word32.word,
                         comment : string,
                         statusBar : string,
                         reserved : Bytestring.string } Reader.reader

   end
