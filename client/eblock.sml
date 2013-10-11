
structure EBlock :> EBLOCK =
   struct

      structure B = Bytestring
      structure BS = Bytesubstring
      structure S = Susp

      type eblock =
         { 
         bytes : B.string,
         block : Block.block S.susp,
         hash : B.string S.susp,
         txcount : int S.susp,
         txhashes : B.string list S.susp
         }

      fun hashBlockHeader str =
         SHA256.hashBytes (SHA256.hash (Stream.fromTable BS.sub (BS.substring (str, 0, 80)) 0))

      val dhash = SHA256.hashBytes o SHA256.hashBytes

      fun fromBytes bytes =
         let
            val block = 
               S.delay (fn () => Block.readBlock bytes)

            val hash =
               S.delay (fn () => hashBlockHeader bytes)

            val txcount =
               S.delay
               (fn () => length (#2 (S.force block)))

            val txhashes =
               S.delay
               (fn () =>
                   map
                   (fn tx => dhash (Transaction.writeTx tx))
                   (#2 (S.force block)))
         in
            { bytes=bytes, block=block, hash=hash, txcount=txcount, txhashes=txhashes }
         end

      fun toBytes ({bytes, ...}:eblock) = bytes

      fun toBlock ({block, ...}:eblock) = S.force block

      fun hash ({hash, ...}:eblock) = S.force hash

      fun txcount ({txcount, ...}:eblock) = S.force txcount

      fun txhashes ({txhashes, ...}:eblock) = S.force txhashes

   end
