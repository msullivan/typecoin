
structure Chain :> CHAIN =
   struct

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

      val bfh = valOf o Bytestring.fromStringHex


      val bitcoin : chain =
         {
         name = "Bitcoin",

         port = 8333,

         testnet = false,

         magic = ConvertWord.word32ToBytesL 0wxd9b4bef9,

         genesisHash =
            bfh "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000",

         genesisBlock =
            bfh "0100000000000000000000000000000000000000000000000000000000000000000000003ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a29ab5f49ffff001d1dac2b7c0101000000010000000000000000000000000000000000000000000000000000000000000000ffffffff4d04ffff001d0104455468652054696d65732030332f4a616e2f32303039204368616e63656c6c6f72206f6e206272696e6b206f66207365636f6e64206261696c6f757420666f722062616e6b73ffffffff0100f2052a01000000434104678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5fac00000000",

         seeds =
            [
            "seed.bitcoin.sipa.be",
            "dnsseed.bluematt.me",
            "dnsseed.bitcoin.dashjr.org",
            "bitseed.xf2.org"
            ],

         blockchainFile = "blockchain.dat",
         
         indexFile = "index.dat",
         
         alertKey =
            ECDERp.decodePubkey (EllipticCurveParams.secp256k1, bfh "04fc9702847840aaf195de8442ebecedf5b095cdbb9bc716bda9110971b28a49e0ead8564ff0db22209e0374782c093bb899692d524e9d6a6956e7c5ecbcd68284")
         }


      val testnet : chain =
         {
         name = "Testnet",

         port = 18333,

         testnet = true,

         magic = ConvertWord.word32ToBytesL 0wx0709110b,

         genesisHash =
            bfh "43497fd7f826957108f4a30fd9cec3aeba79972084e90ead01ea330900000000",

         genesisBlock =
            bfh "0100000000000000000000000000000000000000000000000000000000000000000000003ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4adae5494dffff001d1aa4ae180101000000010000000000000000000000000000000000000000000000000000000000000000ffffffff4d04ffff001d0104455468652054696d65732030332f4a616e2f32303039204368616e63656c6c6f72206f6e206272696e6b206f66207365636f6e64206261696c6f757420666f722062616e6b73ffffffff0100f2052a01000000434104678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5fac00000000",

         seeds =
            [
            "testnet-seed.bitcoin.petertodd.org",
            "testnet-seed.bluematt.me"
            ],

         blockchainFile = "testblockchain.dat",

         indexFile = "testindex.dat",

         alertKey =
            ECDERp.decodePubkey (EllipticCurveParams.secp256k1, bfh "04fc9702847840aaf195de8442ebecedf5b095cdbb9bc716bda9110971b28a49e0ead8564ff0db22209e0374782c093bb899692d524e9d6a6956e7c5ecbcd68284")
         }


      val theChain = ref bitcoin

   end


