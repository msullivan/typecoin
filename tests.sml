
structure Tests =
struct
  open LF Logic TypeCoinTxn TestUtil
  infixr -->

  val nat = c_app "nat" []
  val zero = c_app "z" []
  fun succ n = c_app "s" [n]
  fun plus n1 n2 n3 = c_app "plus" [n1, n2, n3]

  fun v s = HVar (~1, s)
  fun var s = EApp (v s, SNil)

  val [n, m, p, A, B, e, e', D, k, r] =
      map var ["n", "m", "p", "A", "B", "e", "e'", "D", "k", "r"]

  val a_test = FromNamed.convertSg
      [(T, "nat", EType),
       (O, "z", nat),
       (O, "s", arrow nat nat),

       (T, "plus", arrow nat (arrow nat (arrow nat EType))),
       (O, "plus/0",
        EPi ("n", nat, plus zero n n)),
       (O, "plus/s",
        EPi ("m", nat, EPi ("n", nat, EPi ("p", nat,
             arrow (plus m n p)
                   (plus (succ m) n (succ p)))))),

       (T, "commutes",
        EPi ("m", nat, EPi ("n", nat, EPi ("p", nat,
             arrow (plus m n p)
             (arrow
              (plus n m p)
              EType))))),

       (T, "0/commutes",
        EPi ("n", nat, plus n zero n --> EType)),
       (O, "-0",
        c_app "0/commutes"
        [zero,
         c_app "plus/0" [zero]]),
       (O, "-1",
        EPi ("n", nat, EPi ("D", plus n zero n,
             c_app "0/commutes" [n, D] -->
             c_app "0/commutes"
             [succ n,
              c_app "plus/s" [n, zero, n, D]])))
      ]

  val tp = c_app "tp" []
  val term = c_app "term" []
  val base = c_app "base" []
  fun arr t1 t2 = c_app "arr" [t1, t2]
  fun eapp t1 t2 = c_app "app" [t1, t2]
  fun eof e A = c_app "of" [e, A]

  val lambda_test = FromNamed.convertSg
      [(T, "tp", EType),
       (O, "base", tp),
       (O, "arr", tp --> tp --> tp),

       (T, "term", EType),
       (O, "app", term --> term --> term),
       (O, "lam", tp --> (term --> term) --> term),

       (T, "of", term --> tp --> EType),
       (O, "of/app",
        EPi ("A", tp, EPi ("B", tp, EPi ("e", term, EPi ("e'", term,
             eof e (arr A B) --> eof e' A --> eof (eapp e e') B)))))

      ]



  val i = c_app "i" []
  val logic_test_lf_part = FromNamed.convertSg
      [(T, "A", EProp), (T, "B", EProp), (T, "C", EProp),
       (T, "i", EType),
       (T, "P", i --> EProp), (T, "Q", i --> EProp), (T, "R", i --> EProp)]
  val logic_test = map SConst logic_test_lf_part

  val A = PAtom (c_app "A" [])
  val B = PAtom (c_app "B" [])
  val C = PAtom (c_app "C" [])
  fun P x = PAtom (c_app "P" [x])
  fun Q x = PAtom (c_app "Q" [x])
  fun S x = PAtom (c_app "S" [x])

  val [x, y, z, z', w, x1, y1, z1, w1, x2, y2, z2, w2, z1', z2'] =
      map MVar ["x", "y", "z", "z'", "w", "x1", "y1", "z1", "w1", "x2", "y2", "z2", "w2", "z1'", "z2'"]


  (* prove A x B -o B x A *)
  val tensor_comm =
      MLam ("z", PTensor (A, B),
            MTensorLet (z, "z1", "z2",
                        MTensor (z2, z1)))
  (* prove (A -o B -o C) -> (A x B) -o C *)
  val uncurry =
      MLam ("y", PLolli (A, PLolli (B, C)),
       MLam ("z", PTensor (A, B),
        MTensorLet (z, "z1", "z2",
         MApp (MApp (y, z1), z2))))

  (* don't prove A x B -o A & B *)
  val tensor_imp_with =
      MLam ("z", PTensor (A, B),
            MTensorLet (z, "z1", "z2",
                        MWith (z1, z2)))

  (* prove !A x !B -o !A & !B *)
  val tensor_imp_with_bang =
      MLam ("z", PTensor (PBang A, PBang B),
       MTensorLet (z, "z1", "z2",
        MBangLet (z1, "z1'",
        MBangLet (z2, "z2'",
         MWith (MBang z1', MBang z2')))))
  (* don't prove !A & !B -o !A x !B  *)
  val with_imp_tensor_bang_wrong =
      MLam ("z", PWith (PBang A, PBang B),
            MTensor (MPi (L, z), MPi (R, z)))

  (* prove !A x !B -o !(A & B) *)
  val tensor_imp_with_bang2 =
      MLam ("z", PTensor (PBang A, PBang B),
       MTensorLet (z, "z1", "z2",
        MBangLet (z1, "z1'",
        MBangLet (z2, "z2'",
         MBang (MWith (z1', z2'))))))
  (* prove !(A & B) -o !A x !B  *)
  val with_imp_tensor_bang =
      MLam ("z", PBang (PWith (A, B)),
       MBangLet (z, "y",
        MTensor (MBang (MPi (L, y)), MBang (MPi (R, y)))))

  (* don't prove !(A x B) -o !(A & B) *)
  val tensor_imp_with_bang_wrong =
      MLam ("y", PBang (PTensor (A, B)),
       MBangLet (y, "z",
        MTensorLet (z, "z1", "z2",
         MBang (MWith (z1, z2)))))

  (* fail to accept this bogus proof *)
  val tensor_imp_bang =
      MLam ("z", PTensor (PBang A, PBang B),
       MTensorLet (z, "z1", "z2",
        MBangLet (z1, "z1'",
                  MBang z1')))


  val one_lolli_a_equiv_a =
      MWith (
      MLam ("x", PLolli (POne, A),
            MApp (x, MOne)),
      MLam ("y", A,
            MLam ("z", POne,
                  MOneLet (z, y))))

  (* prove A + B -o B + A *)
  val oplus_comm =
      MLam ("z", POplus (A, B),
       MCase (z,
              "z1", MInj (R, z1, POplus (B, A)),
              "z2", MInj (L, z2, POplus (B, A))))

  (* prove A x (B + 0) -o A x B *)
  val thing_with_zero =
      MLam ("z", PTensor (A, POplus (B, PZero)),
       MTensorLet (z, "z1", "z2",
        MCase (z2,
               "x", MTensor (z1, x),
               "y", MAbort (y, PTensor (A, B), ["z1"]))))


  (* prove ((?x:i. P(x)) -o C) => (!x:i. P(x) -o C) *)
  val qcurry = FromNamed.convertProof []
      (MLam ("z", PLolli (PExists ("n", i, P n), C),
        MForallLam ("m", i,
         MLam ("y", P m,
          MApp (z,
                MPack (m, y, PExists ("n", i, P n)))))))

  (* prove A -o B -o T *)
  val top_thing =
      MLam ("y", A,
       MLam ("z", B,
        MTop ["y", "z"]))



  (* prove !(!x:t. A(x) & B(x)) -o (!x:t. A(x)) & (!x:t. B(x)) *)
  (* meh, later. *)


  (* prove (?x:t. P(x) & Q(x)) -o (?x:t. P(x)) & (?x:t. Q(x)) *)
  val distrib_ex_and_1 = FromNamed.convertProof []
      (MLam ("z", PExists ("n", i, PWith (P n, Q n)),
        MUnpack (z, "n", "y",
         MWith (
          MPack (n, MPi (L, y), PExists ("n", i, P n)),
          MPack (n, MPi (R, y), PExists ("n", i, Q n))))))



  (* fail to prove !((?x:t. !P(x)) & (?x:t. !Q(x))) -o (?x:t. P(x) & Q(x)) *)
  val distrib_ex_and_2 = FromNamed.convertProof []
      (MLam ("z", PBang (PWith (PExists ("n", i, PBang (P n)), PExists ("n", i, PBang (Q n)))),
        MBangLet (z, "z'",
         MUnpack (MPi (L, z'), "n", "z1",
         MUnpack (MPi (R, z'), "m", "z2",
         MBangLet (z1, "z1'",
         MBangLet (z2, "z2'",
          MPack (n,
                 MWith (z1', z2'),
                 PExists ("n", i, PWith (P n, Q n))))))))))


  val K = TypeCoinBasis.principal_hash (TypeCoinBasis.test_hash)
  (* prove (A -o B) -o (<K>A -o <K>B) *)
  val affirmation_fmap_specific = FromNamed.convertProof []
      (MLam ("x", PLolli (A, B),
        MLam ("y", PAffirms (K, A),
         MBind (y, "z",
          MReturn (K, MApp (x, z))))))

  (* prove !k:principal. (A -o B) -o (<k>A -o <k>B) *)
  val affirmation_fmap = FromNamed.convertProof []
      (MForallLam ("k", TypeCoinBasis.principal,
        MLam ("x", PLolli (A, B),
         MLam ("y", PAffirms (k, A),
          MBind (y, "z",
           MReturn (k, MApp (x, z)))))))


  (* prove !k:principal. <k><k>A -o <k>A *)
  val affirmation_join = FromNamed.convertProof []
      (MForallLam ("k", TypeCoinBasis.principal,
         MLam ("z", PAffirms (k, PAffirms (k, A)),
          MBind (z, "z1",
           MBind (z1, "z2",
            MReturn (k, z2))))))

  (* fail to prove !k:principal. <k>A -o A *)
  val affirmation_unsafe_perform_io = FromNamed.convertProof []
      (MForallLam ("k", TypeCoinBasis.principal,
         MLam ("z", PAffirms (k, A),
          MBind (z, "z1", z1))))

  (* fail to prove !k, k':principal. <k>A -o <k'>A *)
  val affirmation_coerce = FromNamed.convertProof []
      (MForallLam ("k", TypeCoinBasis.principal,
        MForallLam ("n", TypeCoinBasis.principal,
         MLam ("z", PAffirms (k, A),
          MBind (z, "z1", MReturn (n, z1))))))


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
  val (alice_keypair as (alice_pubkey, alice_privkey)) = test_keypair1
  val alice_hash = hashPubKey alice_pubkey
  val (bob_keypair as (bob_pubkey, bob_privkey)) = test_keypair2
  val bob_hash = hashPubKey bob_pubkey
  val (charlie_keypair as (charlie_pubkey, charlie_privkey)) = test_keypair3
  val charlie_hash = hashPubKey charlie_pubkey


  (* Ok, lets test some transaction stuff. *)
  val P = SRule
  val C = SConst
  structure TB = TypeCoinBasis

  val alice = TB.principal_hash (TB.hashBytestringToHashObj alice_hash)
  val bob = TB.principal_hash (TB.hashBytestringToHashObj bob_hash)
  val charlie = TB.principal_hash (TB.hashBytestringToHashObj charlie_hash)

  (*******************************************************************************************)
  (* First, somebody publishes a transaction with some
   * simple rules about authorization. *)
  local
    (* Set up the initial signature for a simple authorization logic. *)
    val input_txid = "bogus_tx"
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
  val outputs = [Output {dest = charlie_hash, prop = POne, needs_receipt = false}]
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
    val outputs = [Output {dest = charlie_hash, prop = self_persistent_access_prop,
                           needs_receipt = false}]
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
    val outputs = [Output {dest = bob_hash, prop = alice_says_can_access_prop,
                           needs_receipt = false}]
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

    val outputs = [Output {dest = bob_hash, prop = charlie_says_can_access_nonce,
                           needs_receipt = false}]
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

  (*******************************************************************************************)


  (*****************************************************************)

  fun println s = print (s ^ "\n")

  fun check sg =
      (println "";
       println (PrettyLF.prettySg sg);
       ignore (TypeCheckLF.checkSignature sg);
       println "")
      handle (e as TypeCheckLF.TypeError s) => (println s; raise e)

  fun checkProof sg M =
      ((LogicCheck.inferProofOuter sg M)
       handle (e as TypeCheckLF.TypeError s) => (println s; raise e)
            | (e as LogicCheck.ProofError s) => (println s; raise e))

  fun checkChain chain =
      ((TypeCoinCheck.checkChain LogicCheck.basis_sg TxnDict.empty chain)
       handle (e as TypeCheckLF.TypeError s) => (println s; raise e)
            | (e as LogicCheck.ProofError s) => (println s; raise e))

end
