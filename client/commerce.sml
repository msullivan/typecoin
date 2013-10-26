
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
      val secp256k1 = EllipticCurveParams.secp256k1

      fun addressFromPrivkey privkey compress =
         let
            val pubkey = EllipticCurveCryptoFp.privkeyToPubkey (secp256k1, privkey)

            val pubkeystr =
               if compress then
                  ECDERp.encodePubkeyCompressed (secp256k1, pubkey)
               else
                  ECDERp.encodePubkey (secp256k1, pubkey)
         in
            RIPEMD160.hashBytes (SHA256.hashBytes pubkeystr)
         end



      type btcaddr = Bytestring.string  (* A bitcoin address *)

      datatype output =
         Standard of btcaddr

      exception Invalid
      exception NoKey


      fun synthesize output =
         (case output of
             Standard addr =>
                [S.Dup, S.Hash160, S.Const addr, S.EqualVerify, S.Checksig])


      fun analyze l =
         (case l of
             [S.Dup, S.Hash160, S.Const addr, S.EqualVerify, S.Checksig] =>
                Standard addr
           | _ =>
                (* Don't understand this script. *)
                raise Invalid)


      fun createTx { inputs, outputs, fee, keys } =
         let
            val () =
               (* Check that the amounts are in range. *)
               app (fn (_, amount) => 
                          if amount <= 0 orelse amount > maxAmount then
                             raise Invalid
                          else
                             ()) outputs

            val utxo = Blockchain.currentUtxo ()

            val inputs' =
               map 
               (fn (txhash, n) =>
                   (case Blockchain.getTransaction utxo txhash of
                       NONE =>
                          raise Invalid
                     | SOME tx =>
                          let
                             val {amount, script} =
                                List.nth (#outputs tx, n)
                                handle Subscript =>
                                   (* Input transaction doesn't have this many outputs *)
                                   raise Invalid
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
                  raise Invalid

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

            val keydict =
               List.foldl
               (fn (privkey, dict) =>
                   D.insert
                      (D.insert dict (addressFromPrivkey privkey false) (privkey, false))
                      (addressFromPrivkey privkey true) (privkey, true))
               D.empty
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
                                   (case D.find keydict addr of
                                       NONE =>
                                          (* Don't have the key. *)
                                          raise NoKey
                                     | SOME (privkey, compress) =>
                                          let
                                             val sg = Signature.sign pretx j ioscript (Signature.HashAll, false) privkey

                                             val pubkey = ECDSAp.privkeyToPubkey (secp256k1, privkey)
                                             val pubkeystr =
                                                if compress then
                                                   ECDERp.encodePubkeyCompressed (secp256k1, pubkey)
                                                else
                                                   ECDERp.encodePubkey (secp256k1, pubkey)
                                          in
                                             S.writeScript
                                             [S.Const sg, S.Const pubkeystr]
                                          end))

                         val txin = { from=(txinhash, n), script=script, sequence=0wxffffffff }
                      in
                         signloop (j+1) (txin :: acc) rest
                      end)

            val txins = signloop 0 [] inputs'
         in
            Transaction.mkTx { inputs=txins, outputs=txouts, lockTime=0w0 }
         end

   end
