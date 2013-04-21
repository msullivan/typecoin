
signature CHAIN =
   sig

      val name : string
      val port : int
      val magic : Word32.word
      val genesis : Bytestring.string
      val seeds : string list

   end