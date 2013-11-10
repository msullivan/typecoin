
signature CHAIN =
   sig

      type chain =
         {
         name : string,
         port : int,
         testnet : bool,
         magic : Bytestring.string,
         genesisHash : Bytestring.string,
         genesisBlock : Bytestring.string,
         seeds : string list,
         blockchainFile : string,
         indexFile : string,
         alertKey : ECDSAp.pubkey
         }

      val bitcoin : chain
      val testnet : chain

      (* Don't alter this once the program is running.  Defaults to bitcoin. *)
      val theChain : chain ref

   end