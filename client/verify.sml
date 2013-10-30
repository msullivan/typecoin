
structure Verify :> VERIFY =
   struct

      structure B = Bytestring
      structure BS = Bytesubstring
      structure T = Transaction


      (* Constants *)
      val maximumBits : Word32.word = 0wx1d00ffff
      val maximumAmount : IntInf.int = 21000000 * 100000000  (* 21 MBTC, which is slightly more Bitcoin than there will ever be. *)

      (* Precomputed data *)
      val minusone : Word32.word = ~ 0w1
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

         - maximum signature operations (20,000)
         - no repeat transaction unless legacy
         - correct difficulty
         - block version upgrade rule
         - timestamp not too early (GetMedianTimePast in main.h, called in main.cpp)
      *) 


      exception Reject of string

      (* Verify a txin, and return its amount if it passes. *)
      fun verifyTxin enforceP2sh (getTx : T.coord -> T.tx option) tx i ({from=(from as (_, fromIndex)), script=inScript, ...}:T.txin) =
         let
            val () =
               if B.size inScript > Constants.maxScriptSize then
                  raise (Reject "input script too large")
               else
                  ()

            val {outputs, ...} =
               (case getTx from of
                   NONE =>
                      raise (Reject "txin invalid or unavailable")
                 | SOME tx => tx)

            val {amount, script=outScript} =
               (* fromIndex must be within bounds, or getTx would have failed. *)
               List.nth (outputs, fromIndex)

            val () =
               if B.size outScript > Constants.maxScriptSize then
                  raise (Reject "output script too large")
               else
                  ()

            val instack =
               Interpret.exec tx i inScript []
               handle Interpret.Reject => raise (Reject "inscript fails")

            val () =
               if
                  Interpret.passes (Interpret.exec tx i outScript instack)
                  handle Interpret.Reject => false
               then
                  ()
               else
                  raise (Reject "script fails")


            (* This little monstrosity is called Pay to Script Hash (https://en.bitcoin.it/wiki/BIP_0016).
               The aim here is to add a limited eval operation, so that very large output scripts can be
               compressed to a 20-byte hash.  (So that, for example, they can fit in a QR code.) The spender
               supplies the actual script, which is first hashed and compared against the given hash, then
               evaluated.

               So basically they want:

                  DUP HASH160 ..constant.. EQUALVERIFY EVAL

               But there is no EVAL opcode.  Rather than just add one, it was deemed better for the upgrade
               path to identify a particular output script:

                  HASH160 ..constant.. EQUAL

               and treat it as if it were actually the above EVAL script.  This means you run the script as
               usual (thus accomplishing the DUP HASH160 ..constant.. EQUALVERIFY part), and then do the eval.

               You also require that the input script contains only constants, for some reason.

               Pay-to-script-hash came into effect at timestamp 1333238400 (April 1, 2012).  Before that time
               there were (reportedly) transactions that failed the rule.  Thus, we need to be told whether
               pay-to-script-hash is in effect, which is the purpose of enforceP2sh.
            *)
            val () =
               if
                  (* recognize pay-to-script-hash *)
                  B.size outScript = 23
                  andalso
                  B.sub (outScript, 0) = 0wxa9   (* HASH160 *)
                  andalso
                  B.sub (outScript, 1) = 0wx14   (* 20-byte constant *)
                  andalso
                  B.sub (outScript, 22) = 0wx87  (* EQUAL *)
                  andalso
                  (* XX I'm curious if there really were any p2sh-resembling transactions before 1333238400 *)
                  if enforceP2sh
                     then true
                     else (not Chain.testnet) andalso (Log.long (fn () => "Unusual: premature pay-to-script-hash"); false)
               then
                  let
                     (* The input script must contain only constants. *)
                     val () =
                        if List.all Script.isConstant (Script.readScript inScript) then
                           ()
                        else
                           raise (Reject "pay-to-script-hash input script contains non-constants")
                  in
                     (case instack of
                         [] =>
                            (* This should have failed already, but easy enough to do this. *)
                            raise (Reject "empty instack in pay-to-script-hash")
                       | outScript' :: instack' =>
                            if
                               Interpret.passes (Interpret.exec tx i outScript' instack')
                               handle Interpret.Reject => false
                            then
                               ()
                            else
                               raise (Reject "pay-to-script-hash script fails"))
                  end
               else
                  ()
         in
            amount
         end


      (* Verifies the transaction, returns its fee. *)
      fun verifyTxMain enforceP2sh spendTx (tx as { version, inputs, outputs, lockTime }) =
         let
            val () =
               if List.null inputs orelse List.null outputs then
                  raise (Reject "empty inputs or outputs")
               else
                  ()

            val (amountIn, _) = 
               List.foldl
               (fn (txin, (amountAcc, i)) =>
                   let
                      val amount = verifyTxin enforceP2sh spendTx tx i txin
                   in
                      (amountAcc + Word64.toLargeInt amount, i+1)
                   end)
               (0, 0)
               inputs

            val () =
               if amountIn > maximumAmount then
                  raise (Reject "amount in too large")
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
                  raise (Reject "amount out too large")
               else
                  ()

            val fee = amountIn - amountOut

            val () =
               if fee < 0 then
                  raise (Reject "transaction out of balance")
               else
                  ()
         in
            fee : IntInf.int
         end


      fun verifyTx enforceP2sh spendTx tx =
         (verifyTxMain enforceP2sh spendTx tx; true)
         handle Reject _ => false


      type pos = Int64.int


      
      fun coinbaseReward blockNumber =
         IntInf.~>> (5000000000, Word.fromInt (blockNumber div 210000))



      fun isFinalTx blockNumber ({ inputs, lockTime, ...}:T.tx) =
         lockTime = 0w0
         orelse
         let
            val lockTime = Word32.toLargeInt lockTime
            val () = Log.long (fn () => "Unusual: lockTime = "^ LargeInt.toString lockTime)
         in
            (if lockTime < 500000000 then
                LargeInt.fromInt blockNumber > lockTime
             else
                Time.toSeconds (Time.now ()) > lockTime)

            orelse

            List.all (fn {sequence, ...} => sequence = minusone) inputs
         end


      fun verifyStoredBlock getTx utxo blockPos blockNumber eblock =
         let
            (* eblock has already passed verifyBlockGross *)

            val () =
               if B.size (EBlock.toBytes eblock) > Constants.maxBlockSize then
                  raise (Reject "block exceeds maximum size")
               else
                  ()

            val ({timestamp, ...}, _) = EBlock.toBlock eblock

            (* Check that the block's timestamp is not too far in the future.
               This seems dodgy: if a block is just on the margin, mightn't it
               cause a fork because of varying clocks or just the varying time
               at which a node frst sees it?  Nevertheless, this is what the
               reference implementation does.
            *)
            val () =
               if Word32.toLargeInt timestamp > Time.toSeconds (Time.+ (Time.now (), Constants.allowedTimeDrift)) then
                  raise (Reject "timestamp too far in the future")
               else
                  ()

            fun spendTx (coord as (hash, _)) =
               (case getTx utxo hash of
                   NONE => NONE
                 | SOME tx =>
                      if Utxo.spend utxo blockNumber coord then
                         SOME tx
                      else
                         NONE)

            val enforceP2sh = timestamp >= Constants.payToScriptHashTimestamp

            val balance =
               Block.traverseBlock 
               (fn (i, pos, tx, txstr, balance) =>
                   let
                      val hash = SHA256.hashBytes (SHA256.hash (Stream.fromTable BS.sub txstr 0))
                   in
                      if i = 0 then
                         (* Coinbase *)
                         let
                            val {inputs, outputs, ...} = tx

                            (* Check that it has the proper form for a coinbase transaction.
                               The reference implementation also checks that none of the other
                               transactions have this form, but that's not necessary, as they
                               would fail anyway.
                            *)
                            val () =
                               (case inputs of
                                   [{from=(hash, n), script, ...}] =>
                                      if not (B.eq (hash, zeros) andalso n = ~1) then
                                         raise (Reject "ill-formed coinbase transaction")
                                      else if B.size script < 2 orelse B.size script > Constants.maxCoinbaseSize then
                                         raise (Reject "coinbase script too large")
                                      else
                                         ()
                                 | _=>
                                      raise (Reject "ill-formed coinbase transaction"))

                            val amountOut =
                               List.foldl
                               (fn ({amount, script=_}, amountAcc) =>
                                   (amountAcc + Word64.toLargeInt amount))
                               0
                               outputs

                            val () =
                               if amountOut > maximumAmount then
                                  raise (Reject "coinbase amount too large")
                               else
                                  ()
                         in
                            Utxo.insertCoinbase utxo hash (blockPos + Int64.fromInt pos) (length outputs) blockNumber;
                            balance + amountOut
                         end
                      else
                         (* Not coinbase. *)
                         let
                            (* Check that the transaction is final.  The reference implementation does this only as
                               part of a block, not for loose transactions.  Probably this is so that the block number
                               does not have to be made available to transaction checking, but possibly this is so
                               that un-final transactions are passsed around by the peer-to-peer network.
                            *)
                            val () =
                               if isFinalTx blockNumber tx then
                                  ()
                               else
                                  raise (Reject "transacton is not final")

                            val fee =
                               verifyTxMain enforceP2sh spendTx tx
                               handle exn as (Reject _) =>
                                  (
                                  Log.long (fn () => "Verification failure at "^ B.toStringHex (B.rev hash) ^", "^ Int64.toString (blockPos + Int64.fromInt pos));
                                  raise exn
                                  )
                         in
                            Utxo.insert utxo hash (blockPos + Int64.fromInt pos) (length (#outputs tx));
                            balance - fee
                         end
                   end)
               0
               (EBlock.toBytes eblock)

            val () =
               if balance > coinbaseReward blockNumber then
                  raise (Reject "block out of balance")
               else
                  ()
         in
            true
         end
         handle Reject _ => false

   end
