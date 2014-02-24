(* XXX: the testing doesn't work at all with actual btc yet *)
functor BatchTesting(val initial_input : string) =
struct

  open TxnTestHelpers
  val actually_create = false
  val setup = setup actually_create

  infixr -->
  infixr -@
  val (op -@) = PLolli

  fun ifret M = MIfReturn (CTrue, M)

  (* Function to authenticate and then do an action *)
  fun doit f userid =
      (BatchClient.authenticate "127.0.0.1" 5124 userid;
       f)



  local
    val input_txid = initial_input
    (* Set up some silly initial signature. *)
    val inputs = [Input {source = (input_txid, 0), prop = POne}]
    val test_basis = FromNamed.convertLogicBasis
        [(* Resources named by bytestrings *)
         C (T, "A", EProp),
         C (T, "B", EProp),
         C (T, "C", EProp),

         P ("make-C", PAtom (c_app "A" []) -@ PAtom (c_app "B" []) -@ PAtom (c_app "C" [])),
         P ("break-C",
            PAtom (c_app "C" []) -@ PTensor (PAtom (c_app "A" []), PAtom (c_app "B" [])))
        ]
  (* This transaction establishes the rules and gives deposits an A. *)
  val outputs = [StdOutput {dest = janet_hash, prop = PAtom (c_app "C" [])}]
  val proof_term = MLam ("z", PTensor (POne, PAtom (c_app "C" [])),
                         MTensorLet (z, "z1", "z2",
                                      ifret z2))

  in

  val initial_test_txn = [TxnBody
      {name = "initial-test",
       metadata = ["deposit-to=" ^ TypeCoinTxn.toHexId charlie_hash],
       inputs = inputs,
       basis = test_basis,
       linear_grant = [PAtom (c_app "C" [])],
       outputs = outputs,
       proof_term = proof_term}]


  val initial_test_txnid =
      setup "e419b618998b72e146980f3476e99b5178e1be36eac278eeb45dc357743b5a2b"
      (fn _ =>
          TypeCoinCrypto.createTxn [] {
                 typecoin_txn = initial_test_txn,
                 keys = [charlie_privkey],
                 fee = TypeCoinCrypto.baseAmount div 2,
                 recovery_amount = TypeCoinCrypto.baseAmount div 2,
                 recovery_pubkey = charlie_pubkey
          })

  val A = PAtom (c_app' initial_test_txnid "A" [])
  val B = PAtom (c_app' initial_test_txnid "B" [])
  val C = PAtom (c_app' initial_test_txnid "C" [])
  val make_C = MRule (Const.LId initial_test_txnid, "make-C")
  val break_C = MRule (Const.LId initial_test_txnid, "break-C")

  val test_chain = [(NONE, initial_test_txnid, initial_test_txn)]

  end

  (* Send it to the batch server *)
  val C_resid_charlie = doit
                        BatchClient.depositResource charlie_hash test_chain (initial_test_txnid, 0)


  (* Now Charlie breaks C and transfers A to alice and B to Bob *)
  local
    val inputs = [Input {source = (Int32.toString C_resid_charlie, 0), prop = C}]
    val outputs = [StdOutput {dest = alice_hash, prop = A},
                   StdOutput {dest = bob_hash, prop = B}]

    val proof_term =
        MLam ("z", C, ifret (MApp (break_C, z)))

  in

  val charlie_test_txn = TxnBody
      {name = "charlie-C-break",
       metadata = [],
       inputs = inputs,
       basis = [],
       linear_grant = [],
       outputs = outputs,
       proof_term = proof_term}

  end

  (* Send it to the batch server *)
  val (charlie_txnid, [A_resid_alice, B_resid_bob]) =
      doit BatchClient.makeTransaction charlie_hash test_chain charlie_test_txn

  (* Now Alice makes a B -@ C and sends it to Bob *)
  (* For kicks she also sends herself a 1. *)
  local
    val inputs = [Input {source = (Int32.toString A_resid_alice, 0), prop = A}]
    val outputs = [StdOutput {dest = bob_hash, prop = B -@ C},
                   StdOutput {dest = alice_hash, prop = POne}]

    val proof_term =
        MLam ("z", A, ifret (
                      MTensor (MApp (make_C, z),
                               MOne)))

  in

  val alice_test_txn = TxnBody
      {name = "alice-make-lambda",
       metadata = [],
       inputs = inputs,
       basis = [],
       linear_grant = [],
       outputs = outputs,
       proof_term = proof_term}

  end

  (* Send it to the batch server *)

  val (alice_txnid, [B_imp_C_resid_bob, one_resid_alice]) =
      doit BatchClient.makeTransaction alice_hash test_chain alice_test_txn


  (* Build the B now *)
  local
    val inputs = [Input {source = (Int32.toString B_resid_bob, 0), prop = B},
                  Input {source = (Int32.toString B_imp_C_resid_bob, 0), prop = B -@ C}]
    val outputs = [StdOutput {dest = bob_hash, prop = C}]

    val proof_term =
        MLam ("z", PTensor (B, B -@ C),
              MTensorLet (z, "z1", "z2",
                          ifret (MApp (z2, z1))))

  in

  val bob_test_txn = TxnBody
      {name = "bob-make-C",
       metadata = [],
       inputs = inputs,
       basis = [],
       linear_grant = [],
       outputs = outputs,
       proof_term = proof_term}

  end

  (* Send it to the batch server *)
  val (bob_txnid, [C_resid_bob]) =
      doit BatchClient.makeTransaction bob_hash test_chain bob_test_txn
end


