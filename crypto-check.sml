
(* Checking of affirmations in transactions and other crypto stuff. *)
(* I am not super comfortable that I got this crypto stuff right.
 * I am not a crypto person... *)

structure TypeCoinCrypto =
struct
  structure Signing = ECDSAp
  structure Encoding = ECDERp
  val param = EllipticCurveParams.secp256k1


  fun hashKey key = RIPEMD160.hashBytes (SHA256.hashBytes key)
  fun hash data = SHA256.hashBytes data

  (* We need affirmations to be signed relative to the transaction
   * they are in, because we don't want it to be possible to pick up
   * a linear affirmation used in one transaction and introduce it in
   * a new one. (This would make linearity of them meaningless).
   *
   * The set of inputs of a transaction will be unique, so we sign this
   * information along with the prop that is being affirmed.
   *
   * In particular, we sign: hash( hash(inputs) || prop)
   * We use this instead of hash(inputs || prop) because there could
   * concivably exist inputs, inputs', prop, prop' such that
   * inputs || prop == inputs' || prop' but prop != prop'.
   *
   * I really hope this is right. Should we include the principal in the data?
   *)

  (* From the inputs of a transaction, produce some identifying data
   * that we sign alongside a prop. *)
  fun buildTxnIdentifier inputs =
      hash (IOTypes.writeToVector TypeCoinTxn.writeInputs inputs)

  fun buildAffirmationData txnId prop =
      let val propData = IOTypes.writeToVector Logic.writeProp prop
          val data = Bytestring.concat [txnId, propData]
      in hash data end

  fun checkAffirmation txnId {principal, prop, crypto_sig} =
      let val data = buildAffirmationData txnId prop
          val principal = Encoding.decodePubkey (param, principal)
          val crypto_sig = Encoding.decodeSg crypto_sig
          val ok = Signing.verify (param, principal, data, crypto_sig)
      in ok end

  fun makeAffirmation (pubkey, privkey) txnId prop =
      let val data = buildAffirmationData txnId prop
          val crypto_sig = Signing.sign (param, privkey, data)
          val principal = Encoding.encodePubkey (param, pubkey)
          val crypto_sig = Encoding.encodeSg crypto_sig
      in {principal = principal, prop = prop, crypto_sig = crypto_sig} end

end
