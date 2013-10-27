
structure Commerce :> COMMERCE =
   struct

      structure B = Bytestring
      structure BS = Bytesubstring
      structure S = Script
      structure W = Writer

      fun >>> (w, w') = W.seq w w'
      infixr 3 >>>
      

      val maxAmount = Word64.toLargeInt (Word64.~ 0w1)


      structure BytestringOrdered =
         struct
            type t = B.string
            val eq = B.eq
            val compare = B.compare
         end
      structure D = ListDict (structure Key = BytestringOrdered)



      val dhash = SHA256.hashBytes o SHA256.hashBytes
      val dhash20 = RIPEMD160.hashBytes o SHA256.hashBytes
      val secp256k1 = EllipticCurveParams.secp256k1



      datatype output =
         Standard of B.string
       | Multisig of int * B.string list


      exception Analyze
      exception Invalid of string
      exception NoKey


      fun synthesize output =
         (case output of
             Standard addr =>
                [S.Dup, S.Hash160, S.Const addr, S.EqualVerify, S.Checksig]

           | Multisig (m, pubkeys) =>
                S.Constn (IntInf.fromInt m)
                :: map
                      (fn pubkey =>
                          if B.size pubkey < 33 then
                             raise (Invalid "multisig public key too small")
                          else
                             S.Const pubkey)
                      pubkeys 
                @ [S.Constn (IntInf.fromInt (length pubkeys)), S.Checkmultisig])


      fun analyze script =
         (case script of
             [S.Dup, S.Hash160, S.Const addr, S.EqualVerify, S.Checksig] =>
                Standard addr

           | S.Constn m :: rest =>
                (* might be Multisig *)
                let
                   fun loop l acc n =
                      (case l of
                          S.Const pubkey :: rest =>
                             if B.size pubkey < 33 then
                                raise Analyze
                             else
                                loop rest (pubkey :: acc) (n+1)

                        | [S.Constn n', S.Checkmultisig] =>
                             if n = IntInf.toInt n' then
                                Multisig (IntInf.toInt m, rev acc)
                             else
                                raise Analyze

                        | _ =>
                             raise Analyze)
                in
                   loop rest [] 0
                   handle Overflow => raise Analyze
                end

           | _ =>
                raise Analyze)


      fun createTx { inputs, outputs, fee, keys } =
         let
            val () =
               (* Check that the amounts are in range. *)
               app (fn (_, amount) => 
                          if amount <= 0 orelse amount > maxAmount then
                             raise (Invalid "output amount out of range")
                          else
                             ()) outputs

            val utxo = Blockchain.currentUtxo ()

            val inputs' =
               map 
               (fn (txhash, n) =>
                   (case Blockchain.getTransactionByHash utxo txhash of
                       NONE =>
                          raise (Invalid "input transaction not found")
                     | SOME tx =>
                          let
                             val {amount, script} =
                                List.nth (#outputs tx, n)
                                handle Subscript =>
                                   raise (Invalid "input transaction doesn't have this many outputs")
                          in
                             (txhash, n, Word64.toLargeInt amount, script)
                          end))
               inputs

            val () =
               (* Check that input amount = output amount. *)
               if
                  foldl (fn ((_, _, amount, _), total) => amount + total) 0 inputs'
                  =
                  foldl (fn ((_, amount), total) => amount + total) fee outputs
               then
                  ()
               else
                  raise (Invalid "inputs amounts don't match output amounts")

            val pretxins =
               map
               (fn (hash, n, _, _) =>
                   { from=(hash, n), script=B.null, sequence=0wxffffffff })
               inputs'

            val txouts =
               map
               (fn (output, amount) => { amount=Word64.fromLargeInt amount, script=Script.writeScript (synthesize output) })
               outputs

            val pretx : Transaction.tx =
               Transaction.mkTx { inputs=pretxins, outputs=txouts, lockTime=0w0 }

            val (addrDict, pubkeyDict) =
               List.foldl
               (fn (privkey, (addrDict, pubkeyDict)) =>
                   let
                      val pubkey = EllipticCurveCryptoFp.privkeyToPubkey (secp256k1, privkey)

                      val pubkeystr = ECDERp.encodePubkey (secp256k1, pubkey)
                      val pubkeystrComp = ECDERp.encodePubkeyCompressed (secp256k1, pubkey)

                      val addrDict' =
                         D.insert
                            (D.insert addrDict (dhash20 pubkeystr) (pubkeystr, privkey))
                            (dhash20 pubkeystrComp) (pubkeystrComp, privkey)

                      val pubkeyDict' =
                         D.insert
                            (D.insert pubkeyDict pubkeystr privkey)
                            pubkeystrComp privkey
                   in
                      (addrDict', pubkeyDict')
                   end)
               (D.empty, D.empty)
               keys

            fun signloop j acc l =
               (case l of
                   [] => rev acc
                 | (txinhash, n, _, ioscript) :: rest =>
                      let
                         val script =
                            (* Figure out what ioscript is looking for. *)
                            (case analyze (Script.readScript ioscript) of
                                Standard addr =>
                                   (case D.find addrDict addr of
                                       NONE =>
                                          (* Don't have the key. *)
                                          raise NoKey
                                     | SOME (pubkeystr, privkey) =>
                                          let
                                             val sg = Signature.sign pretx j ioscript (Signature.HashAll, false) privkey
                                          in
                                             S.writeScript [S.Const sg, S.Const pubkeystr]
                                          end)

                              | Multisig (m, pubkeys) =>
                                   let
                                      (* m >= 0, n = |pubkeys| *)
                                      fun loop m n pubkeys acc =
                                         if m = 0 then
                                            rev acc
                                         else if m > n then
                                            (* Don't have enough keys. *)
                                            raise NoKey
                                         else
                                            (case pubkeys of
                                                [] =>
                                                   raise (Fail "precondition")
                                              | pubkey :: rest =>
                                                   (case D.find pubkeyDict pubkey of
                                                       NONE =>
                                                          (* Don't have the key. *)
                                                          loop m (n-1) rest acc
                                                     | SOME privkey =>
                                                          let
                                                             val sg = Signature.sign pretx j ioscript (Signature.HashAll, false) privkey
                                                          in
                                                             S.Const sg :: acc
                                                          end))
                                   in
                                      S.writeScript (S.Const B.null :: loop m (length pubkeys) pubkeys [])
                                   end)

                         val txin = { from=(txinhash, n), script=script, sequence=0wxffffffff }
                      in
                         signloop (j+1) (txin :: acc) rest
                      end)

            val txins = signloop 0 [] inputs'
         in
            Transaction.mkTx { inputs=txins, outputs=txouts, lockTime=0w0 }
         end

   end
