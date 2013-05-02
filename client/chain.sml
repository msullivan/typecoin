
structure TestnetChain :> CHAIN =
   struct

      val bfh = valOf o Bytestring.fromStringHex

      val name = "Testnet"
      val port = 18333
      val magic : Word32.word = 0wx0709110b

      val genesisHash =
         bfh "43497fd7f826957108f4a30fd9cec3aeba79972084e90ead01ea330900000000"

      val genesisBlock =
         bfh "0100000000000000000000000000000000000000000000000000000000000000000000003ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4adae5494dffff001d1aa4ae180101000000010000000000000000000000000000000000000000000000000000000000000000ffffffff4d04ffff001d0104455468652054696d65732030332f4a616e2f32303039204368616e63656c6c6f72206f6e206272696e6b206f66207365636f6e64206261696c6f757420666f722062616e6b73ffffffff0100f2052a01000000434104678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5fac00000000"

      val seeds =
         [
         "testnet-seed.bitcoin.petertodd.org",
         "testnet-seed.bluematt.me"
         ]

      val blockchainFile = "testblockchain.dat"

   end


structure BitcoinChain :> CHAIN =
   struct

      val bfh = valOf o Bytestring.fromStringHex

      val name = "Bitcoin"
      val port = 8333
      val magic : Word32.word = 0wxd9b4bef9

      val genesisHash =
         bfh "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000"

      val genesisBlock =
         bfh "0100000000000000000000000000000000000000000000000000000000000000000000003ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a29ab5f49ffff001d1dac2b7c0101000000010000000000000000000000000000000000000000000000000000000000000000ffffffff4d04ffff001d0104455468652054696d65732030332f4a616e2f32303039204368616e63656c6c6f72206f6e206272696e6b206f66207365636f6e64206261696c6f757420666f722062616e6b73ffffffff0100f2052a01000000434104678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5fac00000000"

      val seeds =
         [
         "seed.bitcoin.sipa.be",
         "dnsseed.bluematt.me",
         "dnsseed.bitcoin.dashjr.org",
         "bitseed.xf2.org"
         ]

      val blockchainFile = "blockchain.dat"

   end


structure Chain = BitcoinChain
