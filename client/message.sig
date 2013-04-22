
signature MESSAGE =
   sig

      type word8 = Word8.word
      type word32 = Word32.word
      type word64 = Word64.word

      exception InvalidMessage

      datatype ipaddr = 
         V4 of word8 list  (* length 4 *)
       | V6 of word8 list  (* length 16 *)

      type netaddr =
         {
         services : word64,
         address : ipaddr,
         port : int
         }

      val mkNetaddr : ipaddr -> netaddr

      type version =
         {
         version : int,
         services : word64,
         timestamp : LargeInt.int,
         self : netaddr,
         remote : netaddr,
         nonce : word64,
         agent : string,
         lastBlock : int,
         relay : bool
         }

      val mkVersion :
         { self : netaddr, remote : netaddr, nonce : word64, lastBlock : int } -> version

      type tnetaddr = LargeInt.int * netaddr

      type getblocks =
         {
         version : int,
         hashes : Bytestring.string list,
         lastDesired : Bytestring.string option
         }
         
      val mkGetblocks :
         { hashes : Bytestring.string list, lastDesired : Bytestring.string option } -> getblocks

      datatype objtp = ERR | TX | BLOCK

      type inv = objtp * Bytestring.string

      type rawalert = Bytestring.string * Bytestring.string

      datatype message =
         Version of version
       | Verack
       | Addr of tnetaddr list
       | Inv of inv list
       | Getdata of inv list
       | Notfound of inv list
       | Getblocks of getblocks
       | Tx of Bytesubstring.substring
       | Block of Bytesubstring.substring
       | Getaddr
       | Ping of Word64.word
       | Pong of Word64.word

         (* incoming only *)
       | Alert of rawalert
       | OldVersion of int
       | Unsupported of Bytestring.string
       | Illformed of Bytestring.string

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
