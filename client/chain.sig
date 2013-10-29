
signature CHAIN =
   sig

      val name : string
      val port : int
      val testnet : bool
      val magic : Word32.word
      val genesisHash : Bytestring.string
      val genesisBlock : Bytestring.string
      val seeds : string list
      val blockchainFile : string
      val indexFile : string
      val alertKey : ECDSAp.pubkey

   end