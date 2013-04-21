
structure TestnetChain :> CHAIN =
   struct

      val name = "Testnet"
      val port = 18333
      val magic : Word32.word = 0wx0709110b

      val genesis =
         valOf (Bytestring.fromStringHex "000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943")

      val seeds =
         [
         "testnet-seed.bitcoin.petertodd.org",
         "testnet-seed.bluematt.me"
         ]

   end


structure Chain = TestnetChain
