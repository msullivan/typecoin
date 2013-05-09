
structure Verify (* :> VERIFY *) =
   struct

      structure B = Bytestring
      structure BS = Bytesubstring


      (* Constants *)
      val maximumBits : Word32.word = 0wx1d00ffff

      (* Precomputed data *)
      val zeros = valOf (B.fromStringHex "0000000000000000000000000000000000000000000000000000000000000000")


      exception VerifyFailed


      fun decodeTarget' bits =
         let
            (* We can check this against the minimum difficulty using a simple test on the difficulty
               field, because 0xffff * 256 ^ 0x1d cannot be equalled with a smaller exponent.  Reducing
               the exponent would require 0xffff00 * 256 ^ 0x1c, but we only get 23 bits of mantissa.
            *)
            val () =
               if Word32.> (bits, maximumBits) then
                  raise VerifyFailed
               else
                  ()

            val () =
               if Word32.andb (0wx00800000, bits) = 0w0 then
                  ()
               else
                  (* Negative difficulty *)
                  raise VerifyFailed

            val preexp = ConvertWord.word32ToWord (Word32.>> (bits, 0w24))

            val (exp, mantissa) =
               if preexp >= 0w3 then
                  (Word.toInt (preexp - 0w3), Word32.andb (bits, 0wx7fffff))
               else 
                  (0, Word32.>> (Word32.andb (bits, 0wx7fffff), 0w8 * (0w3 - preexp)))


            val right = BS.substring (zeros, 0, exp)
            val middle = BS.full (ConvertWord.word32ToBytesB mantissa)
            val left = BS.substring (zeros, 0, 28 - exp)
         in
            BS.concat [left, middle, right]
         end

      fun targetToDifficulty target =
         IntInf.<< (1, 0w224) div (ConvertIntInf.fromBytesB target + 1)

      val cachedBits = ref maximumBits
      val cachedTarget = ref (decodeTarget' maximumBits)
      val cachedDifficulty = ref (targetToDifficulty (decodeTarget' maximumBits))

      fun decodeTarget bits =
         (* The difficulty doesn't change often, so cache it. *)
         if bits = !cachedBits then
            !cachedTarget
         else
            let
               val target = decodeTarget' bits
               val diff = targetToDifficulty target
            in
               cachedBits := bits;
               cachedTarget := target;
               cachedDifficulty := diff;
               target
            end

      fun decodeDifficulty bits =
         (* The difficulty doesn't change often, so cache it. *)
         if bits = !cachedBits then
            !cachedDifficulty
         else
            let
               val target = decodeTarget' bits
               val diff = targetToDifficulty target;
            in
               cachedBits := bits;
               cachedTarget := target;
               cachedDifficulty := diff;
               diff
            end



      fun verifyBlockFast blstr =
         let
            val {bits, root, count, transactions, ...} =
               Reader.readfull Block.readBlock (BS.full blstr)
               handle Reader.SyntaxError => raise VerifyFailed

            val () =
               (case Bytestring.compare (B.rev (Block.hashBlockHeader blstr), decodeTarget bits) of
                   GREATER =>
                      raise VerifyFailed
                 | _ => ())

            val () =
               if B.eq (root, Block.merkleRoot count transactions) then
                  ()
               else
                  raise VerifyFailed
         in
            true
         end handle VerifyFailed => false



      (* XXX *)
      fun verifyTx txstr = true


      (* XXX *)
      fun verifyBlock blstr =
         let
            val {count, ...} =
               Reader.readfull Block.readBlock (BS.full blstr)
               handle Reader.SyntaxError => raise VerifyFailed
         in
            (* XXX make it fail a lot, for testing *)
            count mod 3 <> 2
         end

   end



