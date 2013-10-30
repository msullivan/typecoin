structure TxnTests =
struct
  open LF Logic TypeCoinTxn TestUtil
  infixr -->
  fun v s = HVar (~1, s)
  fun var s = EApp (v s, SNil)
  val [n, m, p, A, B, e, e', D, k, r] =
      map var ["n", "m", "p", "A", "B", "e", "e'", "D", "k", "r"]
  val [x, y, z, z', w, x1, y1, z1, w1, x2, y2, z2, w2, z1', z2'] =
      map MVar ["x", "y", "z", "z'", "w", "x1", "y1", "z1", "w1", "x2", "y2", "z2", "w2", "z1'", "z2'"]



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

  type keypair = ECDSAp.pubkey * ECDSAp.privkey
  val (test_keypair1 : keypair as (test_pubkey1, test_privkey1)) =
      (SOME
           (11831032065352454438641876800627675216545526971670677831117729227462543208862,
            115110513735647463363474328864503181748072497052594184213934648437348368030219),
       84949032573639129980743211979748855589646357655829367172829447606736725751911)
  val (test_keypair2 : keypair as (test_pubkey2, test_privkey2)) =
      (SOME
           (58983369042593963632619891589595911832440092793514839008342864010329496907151,
            62162639805088467693464181659153423717612090991394312282516252926635931265248),
       30031085134376089938835666959011487879061968753113189013180106434494274397669)
  val (test_keypair3 : keypair as (test_pubkey3, test_privkey3)) =
      (SOME
           (91414779336211869123681701981829070536738525253602829724283724650164868267290,
            45324093941933083857292792809424115783627380967806617052739798377324684734767),
       63864662182596890716986802929790865809740337433382096343017620806541489714467)
  val (test_keypair4 : keypair as (test_pubkey4, test_privkey4)) =
      (SOME
           (85078306604668984981987122722772113896360372037051247591313875911018671009534,
            82276512876817439049078547073873774935889717301632314030883121023431781113962),
       9058177031455919178826547575639311957926744239246079304451874958948223778964)
  val (alice_keypair as (alice_pubkey, alice_privkey)) = test_keypair1
  val alice_hash = hashPubKey alice_pubkey
  val (bob_keypair as (bob_pubkey, bob_privkey)) = test_keypair2
  val bob_hash = hashPubKey bob_pubkey
  val (charlie_keypair as (charlie_pubkey, charlie_privkey)) = test_keypair3
  val charlie_hash = hashPubKey charlie_pubkey
  val (janet_keypair as (janet_pubkey, janet_privkey)) = test_keypair4
  val janet_hash = hashPubKey janet_pubkey


  (* Ok, lets test some transaction stuff. *)
  val P = SRule
  val C = SConst
  structure TB = TypeCoinBasis

  val alice = TB.principal_hash (TB.hashBytestringToHashObj alice_hash)
  val bob = TB.principal_hash (TB.hashBytestringToHashObj bob_hash)
  val charlie = TB.principal_hash (TB.hashBytestringToHashObj charlie_hash)
  val janet = TB.principal_hash (TB.hashBytestringToHashObj janet_hash)


  (*******************************************************************************************)
  (* First, somebody publishes a transaction with some
   * simple rules about authorization. *)

  local
    val input_txid = "cfe4b9e60d887f59860bdd60bc9c4e0abeabe235ee34ad7e05e3f68e015039eb"
    (* Set up the initial signature for a simple authorization logic. *)
    val inputs = [Input {source = (input_txid, 1), prop = POne}]
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
  val initial_auth_txnid = "auth" (* bogus! *)

  val resource = c_app' initial_auth_txnid "resource" []
  fun resource_named x = c_app' initial_auth_txnid "resource_named" [x]
  fun can_access x = c_app' initial_auth_txnid "can_access" [x]
  fun can_access_nonce x n = c_app' initial_auth_txnid "can_access_nonce" [x, n]
  val use_access = MRule (Const.LId initial_auth_txnid, "use_access")

  val initial_auth_txn = TxnBody
      {inputs = inputs,
       persistent_sg = auth_sg,
       linear_sg = [],
       outputs = outputs,
       proof_term = proof_term}


  val test_resource = resource_named (TB.bytestringToLFBytestring (Bytestring.fromString "foo"))


  fun mk_real_txn () = TypeCoinCrypto.createTxn {
                 typecoin_txn = initial_auth_txn,
                 keys = [charlie_privkey],
                 fee = TypeCoinCrypto.baseAmount div 2,
                 recovery_amount = TypeCoinCrypto.baseAmount div 2,
                 recovery_pubkey = charlie_pubkey
                 }

  end

  (* OK, now Charlie is gonna publish some things:
   * saying that if Alice says somebody can access foo,
   * then Charlie says that.
   * He also gives himself a persistent token giving himself
   * access. He doesn't *really* need this, but it means less
   * signing. *)
  local
    val input_txid = "bogus_tx2"
    val inputs = [Input {source = (input_txid, 0), prop = POne}]
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
          MOneLet (z1,
            z2)))

  in
  val charlie_auth_txnid = "charlie" (* bogus! *)
  val charlie_auth_txn = TxnBody
      {inputs = inputs,
       persistent_sg = sg,
       linear_sg = linear_sg,
       outputs = outputs,
       proof_term = proof_term}
  val charlie_delegates_to_alice =
      MRule (Const.LId charlie_auth_txnid, "charlie_delegates_to_alice")

  end





  (* Now Alice sends a proof to Bob saying he can access a resource. *)

  local
    val input_txid = "bogus_tx3"
    val inputs = [Input {source = (input_txid, 0), prop = POne}]
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
          MOneLet (z1,
            z2)))

  in
  val alice_says_can_access_prop = alice_says_can_access_prop
  val alice_auth_txnid = "alice" (* bogus! *)
  val alice_auth_txn = TxnBody
      {inputs = inputs,
       persistent_sg = sg,
       linear_sg = linear_sg,
       outputs = outputs,
       proof_term = proof_term}
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
    val inputs = [Input {source = (input_txid, 0),
                         prop = alice_says_can_access_prop}]

    val outputs = [StdOutput {dest = bob_hash, prop = charlie_says_can_access_nonce}]
    val sg = []
    val linear_sg = []
    val proof_term =
        MLam ("z", alice_says_can_access_prop,
         MBind (charlie_delegates_to_alice, "y",
          MReturn (
           charlie,
           MApp (
            MForallApp (
             MForallApp (
              use_access,
              test_resource),
             nonce),
            MApp (y, z)))))

  in
  val bob_auth_txnid = "bob" (* bogus! *)
  val bob_auth_txn = TxnBody
      {inputs = inputs,
       persistent_sg = sg,
       linear_sg = linear_sg,
       outputs = outputs,
       proof_term = proof_term}
  end



  val auth_test_chain =
      [(initial_auth_txnid, initial_auth_txn),
       (charlie_auth_txnid, charlie_auth_txn),
       (alice_auth_txnid, alice_auth_txn),
       (bob_auth_txnid, bob_auth_txn)]


end
