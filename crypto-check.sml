
signature TYPE_COIN_CRYPTO =
sig
    structure Signing : ECDSA

    type txn_identifier = Bytestring.string

    val hashKey : Bytestring.string -> Bytestring.string
    val hash : Bytestring.string -> Bytestring.string

    val buildTxnIdentifier : TypeCoinTxn.inputs -> TypeCoinTxn.outputs ->
                             txn_identifier

    val checkAffirmation : txn_identifier
                           -> Logic.signed_affirmation
                           -> bool

    (* If the txn_identifier argument is NONE, produces a persistent affirmation *)
    val makeAffirmation : txn_identifier option
                          -> Signing.pubkey * Signing.privkey
                          -> Logic.prop
                          -> Logic.signed_affirmation

    val baseAmount : IntInf.int

    type txn_specifier =
         {typecoin_txn: TypeCoinTxn.txn_bodies,
          keys: Signing.privkey list,
          fee: LargeInt.int,
          recovery_pubkey: Signing.pubkey,
          recovery_amount: LargeInt.int}


    val createTxn : (TypeCoinTxn.txnid * Transaction.tx) list
                    -> txn_specifier -> Transaction.tx
    val checkTxn : TypeCoinTxn.txn_body list -> Transaction.tx -> unit

    val checkCondition : int -> Logic.condition -> bool
end


(* Checking of affirmations in transactions and other crypto stuff. *)
(* I am not super comfortable that I got this crypto stuff right.
 * I am not a crypto person... *)

structure TypeCoinCrypto : TYPE_COIN_CRYPTO =
struct
  structure Signing = ECDSAp
  structure Encoding = ECDERp
  val param = EllipticCurveParams.secp256k1

  type txn_identifier = Bytestring.string

  val Error = Fail


  fun hashKey key = RIPEMD160.hashBytes (SHA256.hashBytes key)
  fun hash data = SHA256.hashBytes (SHA256.hashBytes data)

  (* We need linear affirmations to be signed relative to the transaction
   * they are in, because we don't want it to be possible to pick up
   * a linear affirmation used in one transaction and introduce it in
   * a new one. (This would make linearity of them meaningless).
   *
   * The set of inputs of a transaction will be unique, so we sign this
   * information along with the prop that is being affirmed.
   *
   * In particular, we sign: hash( hash(inputs || outputs) || prop)
   *
   * OLD NOTE:
   * We use this instead of hash(inputs || prop) because there could
   * concivably exist inputs, inputs', prop, prop' such that
   * inputs || prop == inputs' || prop' but prop != prop'.
   *
   * Actually, that isn't true. The encoding we use of things is self
   * delimiting, so it has to contain information about when the
   * inputs part ends and the prop begins. So it isn't actually
   * conceivable that this could happen. Hashing it still seems
   * pretty reasonable, though.
   *
   * I really hope this is right. Should we include the principal in the data?
   *
   * For persistent affirmations, this isn't true. There we just sign
   * hash(prop).
   *)

  (* From the inputs/outputs of a transaction, produce some identifying data
   * that we sign alongside a prop. *)
  fun buildTxnIdentifier inputs outputs =
      hash (Bytestring.concat [
            IOTypes.writeToVector TypeCoinTxn.writeInputs inputs,
            IOTypes.writeToVector TypeCoinTxn.writeOutputs outputs
      ])

  fun buildAffirmationData txnIdOpt prop =
      let val propData = IOTypes.writeToVector Logic.writeProp prop
          val data = (case txnIdOpt of
                          SOME txnId => Bytestring.concat [txnId, propData]
                        | NONE => propData)
      in hash data end

  fun checkAffirmation txnId {persistent, principal, prop, crypto_sig} =
      let val txnIdOpt = if persistent then NONE else SOME txnId
          val data = buildAffirmationData txnIdOpt prop
          val principal = Encoding.decodePubkey (param, principal)
          val crypto_sig = Encoding.decodeSg crypto_sig
          val ok = Signing.verify (param, principal, data, crypto_sig)
      in ok end

  fun makeAffirmation txnIdOpt (pubkey, privkey) prop =
      let val data = buildAffirmationData txnIdOpt prop
          val crypto_sig = Signing.sign (param, privkey, data)
          val principal = Encoding.encodePubkey (param, pubkey)
          val crypto_sig = Encoding.encodeSg crypto_sig
      in {persistent = not (isSome txnIdOpt),
          principal = principal,
          prop = prop,
          crypto_sig = crypto_sig}
      end

  (* OK, yeah, we'll do the bitcoin stuff here too. *)
  (* What our default quantity of bitcoin we use to represent a resource is. *)
  (* 0.001 btc *)
  val baseAmount: LargeInt.int = 100000
  type txn_specifier =
       {typecoin_txn: TypeCoinTxn.txn_bodies,
        keys: Signing.privkey list,
        fee: LargeInt.int,
        recovery_pubkey: Signing.pubkey,
        recovery_amount: LargeInt.int}

  fun hashTxnBodies txnBody =
      hash (IOTypes.writeToVector TypeCoinTxn.writeTxn_bodies txnBody)

  (* Tau in UTF-8 *)
  val magicNumber = Bytestring.implode [0wxCF, 0wx84]

  fun makeFakePubkey typecoin_txn =
      Bytestring.concat [
        magicNumber,
        hashTxnBodies typecoin_txn
      ]

  (* txns is a list of transactions that we might want to reference
   * that haven't landed yet *)
  fun createTxn txns
                (txn: txn_specifier as
                 {typecoin_txn, keys, fee, recovery_pubkey, recovery_amount}) =
      let val (TypeCoinTxn.TxnBody {inputs, outputs, ...}) = hd typecoin_txn

          fun convertInput (TypeCoinTxn.Input {source = (txnid, i), ...}) =
              (Bytestring.rev (valOf (Bytestring.fromStringHex txnid)), i)
          fun convertOutput (TypeCoinTxn.Output {dest, amount, ...}) =
              (Commerce.PayToKeyHash dest,
               getOpt (amount, baseAmount))

          val fakePubKey = makeFakePubkey typecoin_txn
          val realPubKey = Encoding.encodePubkey (param, recovery_pubkey)
          val fakeOutput = (Commerce.Multisig (1, [fakePubKey, realPubKey]),
                            recovery_amount)

          val inputs' = map convertInput inputs
          val outputs' = map convertOutput outputs

          fun lookup id =
              (case List.find (fn (id', _) => TypeCoinTxn.fromHexId id' = id) txns of
                   SOME (_, txn) => SOME txn
                 (* if we don't have it, do an rpc lookup *)
                 | NONE => RPC.Blockchain.tx id)

      in Commerce.createTx lookup {
           inputs = inputs',
           outputs = outputs' @ [fakeOutput],
           fee = fee,
           keys = keys
         }
      end

  (* Checks a typecoin transaction against a bitcoin transaction *)
  fun checkTxn tcTxn (realTxn: Transaction.tx) =
      let val {inputs=realInputs, outputs=realOutputs, ...} = realTxn
          val (TypeCoinTxn.TxnBody {inputs=tcInputs, outputs=tcOutputs, ...}) = hd tcTxn

          val (hashTxout :: revTxouts) = rev realOutputs
          val regularTxouts = rev revTxouts

          val hashOutput = Commerce.analyze (Script.readScript (#script hashTxout))
          val fakePubkey = (case hashOutput of
                                Commerce.Multisig (1, [fakePubkey, _]) => fakePubkey
                              | _ => raise Error "real txn doesn't have typecoin hash")
          val () = if fakePubkey = makeFakePubkey tcTxn then () else
                   raise Error "transaction hash doesn't match!"

          fun checkInput (TypeCoinTxn.Input {source=(txnid, n), ...},
                          {from=(txnid', n'), ...} : Transaction.txin) =
              if TypeCoinTxn.fromHexId txnid = txnid' andalso n = n' then () else
              raise Error "transaction inputs don't match"

          val () = ListPair.appEq checkInput (tcInputs, realInputs)

          fun checkOutput (TypeCoinTxn.Output {dest, amount=amountOpt, ...},
                          {script, amount=amount', ...} : Transaction.txout) =
              let val dest' = (case Commerce.analyze (Script.readScript script) of
                                   Commerce.PayToKeyHash dest' => dest'
                                 | _ => raise Error "nonstandard output")
              in (if dest = dest' then () else
                  raise Error "transaction destinations don't match";
                  (case amountOpt of
                       SOME amount => if amount = Word64.toLargeInt amount' then () else
                                      raise Error "transaction amounts don't match"
                     | NONE => ()))
              end

          val () = ListPair.appEq checkOutput (tcOutputs, regularTxouts)

      in () end

  (* Verify that `txid' in `block' actually spent the output `coord' *)
  (* We get this data from blockexplorer, but "trust but verify" *)
  (* TODO: actually do this check *)
  fun verifySpending coord txid block = ()

  (* A nice thing to do would be to collect *why* a condition
   * didn't hold for error message purposes. *)
  fun checkCondition t c =
      (case c of
           Logic.CTrue => true
         | Logic.CAnd (c1, c2) => checkCondition t c1 andalso checkCondition t c2
         | Logic.CNot c => not (checkCondition t c)
         | Logic.CBefore t' => t < TypeCoinBasis.lfNumToInt t'
         | Logic.CSpent c =>
           let val coord = TypeCoinBasis.lfCoordToCoord c
           in not (RPC.Blockchain.isUnspent coord) andalso
              let val (txid, block) = valOf (BlockExplorer.getSpendingTxAndBlock coord)
                      handle _ => raise Fail "looking up spending block failed"
                  val () = verifySpending coord txid block
              in block < t end
           end
      )


end
