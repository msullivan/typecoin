
structure Verify (* :> VERIFY *) =
   struct

      structure B = Bytestring
      structure BS = Bytesubstring
      structure T = Transaction


      (* Constants *)
      val maximumBits : Word32.word = 0wx1d00ffff
      val maximumAmount : IntInf.int = Word64.toLargeInt (~ 0w1)

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



      fun dhash2 str1 str2 = SHA256.hashBytes (SHA256.hashBytes (B.^ (str1, str2)))

      (* merkle n l: if  n = |l| and n > 0  then  return the merkle root of l *)
      fun merkleRoot n l =
         let
            fun double f l =
               let
                  val (h1, l') = f l
               in
                  (case l' of
                      nil =>
                         (* out of elements, duplicate h1 *)
                         (dhash2 h1 h1, nil)
                    | _ :: _ =>
                         let
                            val (h2, l'') = f l'
                         in
                            (dhash2 h1 h2, l'')
                         end)
               end
      
            fun loop n i f =
               if i >= n then
                  f
               else
                  loop n (i*2) (double f)
         in
            #1 (loop n 1 (fn [] => raise (Fail "no elements") | h :: t => (h, t)) l)
         end



      fun verifyBlockGross eblock =
         let
            val ({bits, root, ...}, transactions) =
               EBlock.toBlock eblock
               handle Reader.SyntaxError =>
                  (
                  Log.long (fn () => "Verify parse failed");
                  raise VerifyFailed
                  )

            val () =
               (case Bytestring.compare (B.rev (EBlock.hash eblock), decodeTarget bits) of
                   GREATER =>
                      (
                      Log.long (fn () => "Verify difficulty failed");
                      raise VerifyFailed
                      )
                 | _ => ())

            val () =
               if B.eq (root, merkleRoot (EBlock.txcount eblock) (EBlock.txhashes eblock)) then
                  ()
               else
                  (
                  Log.long (fn () => "Verify merkle root failed");
                  raise VerifyFailed
                  )
         in
            true
         end handle VerifyFailed =>
            let
               val path = OS.Path.concat (Constants.dataDirectory, "offending-block")
               val outs = BinIO.openOut path
            in
               BinIO.output (outs, EBlock.toBytes eblock);
               BinIO.closeOut outs;
               false
            end


      (* XXXX Things that still need to be verified:

         - timestamp
         - allowable difficulty
         - maximum signature operations (20,000)
         - pay to script hash (@#$%&!)
         - coinbase maturity
         - coinbase amount!
         - the first transaction (and only the first transaction) has zero for its txin "hash"
         - no repeat transaction unless legacy
      *) 


      exception Reject = Interpret.Reject

      (* Verify a txin, and return its amount if it passes. *)
      fun verifyTxin (getTx : T.coord -> T.tx option) tx i ({from=(from as (_, fromIndex)), script=inScript, ...}:T.txin) =
         let
            val () =
               if B.size inScript > Constants.maxScriptSize then
                  raise Reject
               else
                  ()

            val {outputs, ...} =
               (case getTx from of
                   NONE =>
                      raise Reject
                 | SOME tx => tx)

            val {amount, script=outScript} =
               (* fromIndex must be within bounds, or getTx would have failed. *)
               List.nth (outputs, fromIndex)

            val () =
               if B.size outScript > Constants.maxScriptSize then
                  raise Reject
               else
                  ()

            val () =
               if Interpret.passes (Interpret.exec tx i outScript (Interpret.exec tx i inScript [])) then
                  ()
               else
                  raise Reject

            (* XX Need to handle @#$%&! pay-to-script-hash *)
         in
            amount
         end


      (* Verifies the transaction, returns its fee. *)
      fun verifyTxMain getTx (tx as { version, inputs, outputs, lockTime }) =
         let
            val (amountIn, _) = 
               List.foldl
               (fn (txin, (amountAcc, i)) =>
                   let
                      val amount = verifyTxin getTx tx i txin
                   in
                      (amountAcc + Word64.toLargeInt amount, i+1)
                   end)
               (0, 0)
               inputs

            val () =
               if amountIn > maximumAmount then
                  raise Reject
               else
                  ()

            val amountOut =
               List.foldl
               (fn ({amount, script=_}, amountAcc) =>
                   (amountAcc + Word64.toLargeInt amount))
               0
               outputs

            val () =
               if amountOut > maximumAmount then
                  raise Reject
               else
                  ()

            val fee = amountIn - amountOut

            val () =
               if fee < 0 then
                  raise Reject
               else
                  ()
         in
            fee : IntInf.int
         end


      fun verifyTx getTx tx =
         (verifyTxMain getTx tx; true)
         handle Reject => false


      type pos = Int64.int



      fun verifyStoredBlock getTx utxo blockPos eblock =
         let
            (* eblock has already passed verifyBlockGross *)

            val () =
               if B.size (EBlock.toBytes eblock) > Constants.maxBlockSize then
                  raise Reject
               else
                  ()

            fun spendTx (coord as (hash, _)) =
               (case getTx utxo hash of
                   NONE => NONE
                 | SOME tx =>
                      if Utxo.spend utxo coord then
                         SOME tx
                      else
                         NONE)

            val fees =
               Block.traverseBlock 
               (fn (i, pos, tx, txstr, fees) =>
                   let
                      val hash = SHA256.hashBytes (SHA256.hash (Stream.fromTable BS.sub txstr 0))

                      (* Verify the transaction, unless it's coinbase. *)
                      
                      val fee =
                         if i = 0 then
                            0
                         else
                            verifyTxMain spendTx tx
                            handle Reject =>
                               (
                               Log.long (fn () => "Verification failure at "^ B.toStringHex (B.rev hash) ^", "^ Int64.toString (blockPos + Int64.fromInt pos));
                               raise Reject
                               )
                   in
                      Utxo.insert utxo hash (blockPos + Int64.fromInt pos) (length (#outputs tx));
                      fees + fee
                   end)
               0
               (EBlock.toBytes eblock)

            val () =
               if fees > maximumAmount then
                  raise Reject
               else
                  ()
         in
            true
         end
         handle Reject => false

   end
