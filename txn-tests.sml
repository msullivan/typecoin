structure TxnTestHelpers =
struct
  (* Lurr. *)
  val _ = Signals.setHandler (UnixSignals.sigPIPE, Signals.IGNORE)




  open LF Logic TypeCoinTxn TestUtil
  fun v s = HVar (~1, s)
  fun var s = EApp (v s, SNil)
  val [n, m, p, A, B, e, e', D, k, r] =
      map var ["n", "m", "p", "A", "B", "e", "e'", "D", "k", "r"]
  val [x, y, z, z', w, x1, y1, z1, w1, x2, y2, z2, w2, z1', z2'] =
      map MVar ["x", "y", "z", "z'", "w", "x1", "y1", "z1", "w1", "x2", "y2", "z2", "w2", "z1'", "z2'"]
  val [N, M, Q, N', M', K, Ti] =
      map var ["N", "M", "Q", "N", "M", "K", "Ti"]

  structure TB = TypeCoinBasis



  (*****************************************************************)
  (* Some standard test keypairs. *)
  structure Signing = ECDSAp
  structure Encoding = ECDERp
  val param = EllipticCurveParams.secp256k1
  fun hashPubKey key = TypeCoinCrypto.hashKey (Encoding.encodePubkey (param, key))

  fun affirmationProp pubkey prop =
      LogicCheck.affirmationToProp
          {principal = Encoding.encodePubkey (param, pubkey),
           prop = prop,
           crypto_sig = Bytestring.null}

  fun StdOutput {dest, prop} = Output {dest = dest, prop = prop,
                                       needs_receipt = false, amount = NONE}

  fun makeTxnId real_txn =
      Bytestring.toStringHex (Bytestring.rev (TypeCoinCrypto.hash (Transaction.writeTx real_txn)))

  type keypair = ECDSAp.pubkey * ECDSAp.privkey

  fun makeKeyStuff' privkey =
      let val pubkey = EllipticCurveCryptoFp.privkeyToPubkey (param, privkey)
          val hash = hashPubKey pubkey
          val id = TB.principal_hash (TB.hashBytestringToHashObj hash)
      in ((pubkey, privkey), pubkey, privkey, hash, id) end
  fun makeKeyStuff privkey_text =
      let val (privkey, bs) = Textcode.decodePrivkeyTestnet privkey_text
      in makeKeyStuff' privkey end

  val (alice_keypair, alice_pubkey, alice_privkey, alice_hash, alice) =
      makeKeyStuff' 84949032573639129980743211979748855589646357655829367172829447606736725751911
  val (bob_keypair, bob_pubkey, bob_privkey, bob_hash, bob) =
      makeKeyStuff' 30031085134376089938835666959011487879061968753113189013180106434494274397669
  val (charlie_keypair, charlie_pubkey, charlie_privkey, charlie_hash, charlie) =
      makeKeyStuff' 63864662182596890716986802929790865809740337433382096343017620806541489714467
  val (janet_keypair, janet_pubkey, janet_privkey, janet_hash, janet) =
      makeKeyStuff' 9058177031455919178826547575639311957926744239246079304451874958948223778964


  (* Ok, lets test some transaction stuff. *)
  val P = SRule
  val C = SConst


  (*********** Set up a transaction *************)



  (*******************************************************************************************)
  (* First, somebody publishes a transaction with some
   * simple rules about authorization. *)

  val pending: (string * Transaction.tx) list ref = ref nil
  fun register id txn = pending := (id, txn) :: !pending

  fun setup actually_create id f =
      if actually_create then
          let val real_txn = f ()
              val id = makeTxnId real_txn
              val () = register id real_txn
          in id end
      else id


end


functor TxnTests(Inputs : sig
                     val initial_input_txid : string
                     val alice_input_txid : string
                     val bob_input_txid : string
                     val charlie_input_txid : string
                 end) =
struct

  open Inputs TxnTestHelpers
  val actually_create = false
  val setup = setup actually_create

  infixr -->


  local
    val input_txid = initial_input_txid
    (* Set up the initial signature for a simple authorization logic. *)
    val inputs = [Input {source = (input_txid, 0), prop = POne}]
    val resource' = c_app "resource" []
    val nonce = TB.hash256
    val auth_sg = FromNamed.convertLogicSg
        [(* Resources named by bytestrings *)
         C (T, "resource", EType),
         C (O, "resource_named", TB.bytestring --> resource'),

         C (T, "can_access", resource' --> EProp),
         C (T, "can_access_nonce", resource' --> nonce --> EProp),

         (* If we have an access permission, we can stamp it with a nonce. *)
         P ("use_access",
            PForall ("r", resource',
             PForall ("n", nonce,
              PLolli (PAtom (c_app "can_access" [r]),
                      PAtom (c_app "can_access_nonce" [r, n])))))
        ]
  (* This transaction just establishes the rules. No useful outputs. *)
  val outputs = [StdOutput {dest = charlie_hash, prop = POne}]
  val proof_term = MLam ("z", POne, z)

  in

  val initial_auth_txn = TxnBody
      {name = "initial-auth",
       metadata = [],
       inputs = inputs,
       persistent_sg = auth_sg,
       linear_sg = [],
       outputs = outputs,
       proof_term = proof_term}


  val initial_auth_txnid =
      setup "e419b618998b72e146980f3476e99b5178e1be36eac278eeb45dc357743b5a2b"
      (fn _ =>
          TypeCoinCrypto.createTxn [] {
                 typecoin_txn = initial_auth_txn,
                 keys = [charlie_privkey],
                 fee = TypeCoinCrypto.baseAmount div 2,
                 recovery_amount = TypeCoinCrypto.baseAmount div 2,
                 recovery_pubkey = charlie_pubkey
          })

  val resource = c_app' initial_auth_txnid "resource" []
  fun resource_named  x = c_app' initial_auth_txnid "resource_named" [x]
  fun can_access x = c_app' initial_auth_txnid "can_access" [x]
  fun can_access_nonce x n = c_app' initial_auth_txnid "can_access_nonce" [x, n]
  val use_access = MRule (Const.LId initial_auth_txnid, "use_access")

  val test_resource = resource_named (TB.bytestringToLFBytestring (Bytestring.fromString "foo"))


  end

  (* OK, now Charlie is gonna publish some things:
   * saying that if Alice says somebody can access foo,
   * then Charlie says that.
   * He also gives himself a persistent token giving himself
   * access. He doesn't *really* need this, but it means less
   * signing. *)
  local
    val inputs = [Input {source = (charlie_input_txid, 1), prop = POne}]
    val self_persistent_access_prop =
        affirmationProp charlie_pubkey (PBang (PAtom (can_access test_resource)))
    val outputs = [StdOutput {dest = charlie_hash, prop = self_persistent_access_prop}]
    val txn_ident = TypeCoinCrypto.buildTxnIdentifier inputs outputs


    val delegate_to_alice =
        TypeCoinCrypto.makeAffirmation charlie_keypair txn_ident
        (PLolli (PAffirms (alice, PAtom (can_access test_resource)),
                 PAtom (can_access test_resource)))
    val self_persistent_access =
        TypeCoinCrypto.makeAffirmation charlie_keypair txn_ident
        (PBang (PAtom (can_access test_resource)))

    val sg = [
        SSignedAffirmation ("charlie_delegates_to_alice", delegate_to_alice)
    ]
    val linear_sg = [
        LSSignedAffirmation self_persistent_access
    ]
    val proof_term =
        MLam ("z", PTensor (POne, self_persistent_access_prop),
         MTensorLet (z, "z1", "z2",
            z2))

  in

  val charlie_auth_txn = TxnBody
      {name = "charlie-auth",
       metadata = [],
       inputs = inputs,
       persistent_sg = sg,
       linear_sg = linear_sg,
       outputs = outputs,
       proof_term = proof_term}

  val charlie_auth_txnid =
      setup "c919d1f954d384e019e13bc5632ceb9e924362915d98f02a5218d3689cdcb6b2"
      (fn _ =>
          TypeCoinCrypto.createTxn (!pending) {
                 typecoin_txn = charlie_auth_txn,
                 keys = [charlie_privkey],
                 fee = TypeCoinCrypto.baseAmount div 2,
                 recovery_amount = TypeCoinCrypto.baseAmount div 2,
                 recovery_pubkey = charlie_pubkey
          })

  val charlie_delegates_to_alice =
      MRule (Const.LId charlie_auth_txnid, "charlie_delegates_to_alice")

  end


  (* Now Alice sends a proof to Bob saying he can access a resource. *)
  local
    val inputs = [Input {source = (alice_input_txid, 1), prop = POne}]
    val alice_says_can_access_prop =
        affirmationProp alice_pubkey (PAtom (can_access test_resource))
    val outputs = [StdOutput {dest = bob_hash, prop = alice_says_can_access_prop}]
    val txn_ident = TypeCoinCrypto.buildTxnIdentifier inputs outputs


    val alice_says_can_access =
        TypeCoinCrypto.makeAffirmation alice_keypair txn_ident
        (PAtom (can_access test_resource))

    val sg = []
    val linear_sg = [
        LSSignedAffirmation alice_says_can_access
    ]
    val proof_term =
        MLam ("z", PTensor (POne, alice_says_can_access_prop),
         MTensorLet (z, "z1", "z2",
            z2))

  in
  val alice_says_can_access_prop = alice_says_can_access_prop
  val alice_auth_txn = TxnBody
      {name = "alice-auth",
       metadata = [],
       inputs = inputs,
       persistent_sg = sg,
       linear_sg = linear_sg,
       outputs = outputs,
       proof_term = proof_term}

  val alice_auth_txnid =
      setup "b4ec90da38d8346b03c6fa769fcbff03488a532d6519403599b0faa8c778c7ba"
      (fn _ =>
          TypeCoinCrypto.createTxn (!pending) {
                 typecoin_txn = alice_auth_txn,
                 keys = [alice_privkey],
                 fee = TypeCoinCrypto.baseAmount div 2,
                 recovery_amount = TypeCoinCrypto.baseAmount div 2,
                 recovery_pubkey = alice_pubkey
      })
  end


  (* Now Bob proves he can access it. *)
  local
    val nonce_s = "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592"
    val nonce = TypeCoinBasis.hashStringToHashObj nonce_s

    val can_access_nonce =
        (PAtom (can_access_nonce test_resource nonce))
    val charlie_says_can_access_nonce =
        affirmationProp charlie_pubkey can_access_nonce

    val input_txid = alice_auth_txnid
    val inputs = [Input {source = (alice_auth_txnid, 0),
                         prop = alice_says_can_access_prop},
                  Input {source = (bob_input_txid, 1),
                         prop = POne}]

    val outputs = [StdOutput {dest = bob_hash, prop = charlie_says_can_access_nonce}]
    val sg = []
    val linear_sg = []
    (* This doesn't need to be done as a proof exp but I figured at least one should be. *)
    val proof_term =
        MLam ("z", PTensor (alice_says_can_access_prop, POne),
         MTensorLet (z, "z1", "z2",
         MSayBind (charlie_delegates_to_alice, "y",
          MSayReturn (
           charlie,
           MApp (
            MForallApp (
             MForallApp (
              use_access,
              test_resource),
             nonce),
            MApp (y, z1))))))

  in
  val bob_auth_txn = TxnBody
      {name = "bob-auth",
       metadata = [],
       inputs = inputs,
       persistent_sg = sg,
       linear_sg = linear_sg,
       outputs = outputs,
       proof_term = proof_term}

  val bob_auth_txnid =
      setup "4b827a45e03b6fe2145e1f24d63645d9826a9320836d3e560fa3eb1e2af2d1a2"
      (fn _ =>
          TypeCoinCrypto.createTxn (!pending) {
                 typecoin_txn = bob_auth_txn,
                 keys = [bob_privkey, bob_privkey],
                 fee = TypeCoinCrypto.baseAmount div 2,
                 recovery_amount = TypeCoinCrypto.baseAmount div 2 * 3,
                 recovery_pubkey = bob_pubkey
      })

  end


  val auth_test_chain =
      [(initial_auth_txnid, initial_auth_txn),
       (charlie_auth_txnid, charlie_auth_txn),
       (alice_auth_txnid, alice_auth_txn),
       (bob_auth_txnid, bob_auth_txn)]

  fun submit () = List.app (fn (_, tx) => RPC.Process.inject tx) (rev (!pending))


end


structure MoneyTests =
struct

  open TxnTestHelpers
  val actually_create = false
  val setup = setup actually_create

  infixr -->
  infixr -@
  val (op -@) = PLolli

  fun nus e = PExists ("_", e, POne)

  val num = TypeCoinBasis.number
  val time = TypeCoinBasis.number
  val plus = TypeCoinBasis.plus
  val principal = TypeCoinBasis.principal

  fun money' n = PAtom (c_app "money" [n])
  fun issue' n = PAtom (c_app "issue" [n])
  fun is_banker' k t = PAtom (c_app "is_banker" [k, t])

  (* Alice is the president, or something. *)
  val president = alice

  val money_sg = FromNamed.convertLogicSg
        [(* Simple things about manipulating money. *)
         C (T, "money", num --> EProp),
         P ("zero_money", money' (TypeCoinBasis.intToLFNumber 0)),
         P ("redistribute",
            PForall ("N", num, PForall ("M", num,
            PForall ("N'", num, PForall ("M'", num,
            PForall ("Q", num,
                 nus (plus N M Q) -@ nus (plus N' M' Q) -@
                 PTensor (money' N, money' M) -@
                 PTensor (money' N', money' M'))))))),

         (* Introducing money, central banker. *)
         C (T, "is_banker",  principal --> time --> EProp),
         C (T, "appointment",  principal --> time --> EProp),
         P ("appoint",
            PForall ("K", principal, PForall ("Ti", time,
              PAffirms (president, PAtom (c_app "appointment" [K, Ti])) -@
              PAtom (c_app "is_banker" [K, Ti])))),

         C (T, "issue",  num --> EProp),
         P ("print",
            PForall ("K", principal, PForall ("Ti", time, PForall ("N", num,
             PAtom (c_app "is_banker" [K, Ti]) -@
             PAffirms (K, issue' N) -@
             PIf (CBefore Ti, money' N)))))
        ]


end



structure ParamThings =
struct
  val initial_input_txid = "2d93d4c866f3fd7b4738717020d7f750b3f01c425df3b91ffecce6abf5348310"
  val charlie_input_txid = "5bec004110726e8619690d843685395ecf118d1fc2a0cb45ecabe194bacbe14b"
  val alice_input_txid = "3a2a766f9b2ca4a240053438a8f973fcaf27ed0f0eec4e76153b60944bb049dc"
  val bob_input_txid = "727b0afde4b0bf560a3369be60253706ade8ec82b69c8e65d12cfab033981069"
end


