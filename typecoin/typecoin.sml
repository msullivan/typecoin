
signature TYPE_COIN_CHECK =
sig
  exception TypeCoinError of string

  type type_record = Logic.prop vector TypeCoinTxn.TxnDict.dict

  val lookupTxout : type_record -> TypeCoinTxn.txnid * int ->
                    Logic.prop

  val checkTransaction : Basis.basis -> type_record -> TypeCoinTxn.txn ->
                         (Basis.basis * type_record)
  val checkChain : Basis.basis -> type_record -> TypeCoinTxn.chain ->
                   (Basis.basis * type_record)
end


structure TypeCoinCheck : TYPE_COIN_CHECK =
struct
  local
      open TypeCoinTxn Logic

  in

  exception TypeCoinError of string

  type type_record = Logic.prop vector TxnDict.dict

  fun build_tensor [] = POne
    | build_tensor [x] = x
    | build_tensor (x::xs) = PTensor (x, build_tensor xs)

  fun lookupTxout tr (source_txn, idx) =
      (case TxnDict.find tr source_txn of
           (* Not a typecoin txn. Type must be 1 *)
            NONE => POne
          | SOME source_outputs => Vector.sub (source_outputs, idx))


  fun checkInput tr (Input {source, prop, ...}) =
      (LogicCheck.propEquality prop (lookupTxout tr source);
       prop)

  fun checkInputs tr inputs = map (checkInput tr) inputs

  fun checkOutput basis (Output {dest, prop, needs_receipt, ...}) =
      let val () = LogicCheck.checkProp basis LogicContext.empty prop
          val lf_hash = TypeCoinStdlib.hashBytestringToHashObj dest
          val receipt =
              if needs_receipt then
                  SOME (PReceipt (TypeCoinStdlib.address_hash lf_hash,
                                  prop))
              else NONE
      in (prop, receipt) end

  fun checkOutputs basis outputs = ListPair.unzip (map (checkOutput basis) outputs)

  fun checkLinearGrantEntry basis prop =
      (LogicCheck.checkProp basis LogicContext.empty prop;
       LogicCheck.thawedProp prop;
       prop)

  fun checkLinearGrant basis linear_grant = map (checkLinearGrantEntry basis) linear_grant

  val debug_prop_pair = ref (POne, POne)

  fun checkTransaction' basis tr
                       (block,
                        txnid,
                        TxnBody {inputs, basis=persistent_basis, linear_grant, outputs, proof_term,
                                 name, ...}) =
      let val () = print ("checking " ^ txnid ^ "/" ^ name ^ "\n")

          (* Check the inputs and the outputs and the signatures and build up
           * the data structures we need to check the proof term. *)
          val input_props = checkInputs tr inputs
          val basis' = LogicCheck.checkBasis basis persistent_basis
          val linear_grant_props = checkLinearGrant basis' linear_grant
          val (output_props, receipt_props) = checkOutputs basis' outputs
          val receipt_props = List.mapPartial (fn x => x) receipt_props

          (* Build up the prop that we need to prove. *)
          val input_prop = build_tensor (input_props @ linear_grant_props @ receipt_props)
          val output_prop = build_tensor output_props

          (* Build a digest of the transaction for affirmation checking *)
          val txn_ident = TypeCoinCrypto.buildTxnIdentifier inputs outputs

          (* Moment of truth: check the proof term. *)
          val actual_prop = LogicCheck.inferProofOuter
                                txn_ident
                                basis'
                                LogicContext.empty
                                proof_term
          val (actual_input, actual_output, condition) =
              (case actual_prop of
                   PLolli (A, PIf (c, B)) => (A, B, c)
                 | _ => raise TypeCoinError "proof term has bogus type")

          val () = debug_prop_pair := (actual_input, input_prop)
          val () = LogicCheck.propEquality actual_input input_prop
          val () = debug_prop_pair := (actual_output, output_prop)
          val () = LogicCheck.propEquality actual_output output_prop

(*
          val () = print (
                   Layout.tostring (
                   Layout.seq [Layout.str "assuming condition: ",
                               PrettyLogic.toLayoutCondition condition]) ^ "\n")
*)
          (* If we don't have a particular block number, we'll just assume the current
           * one for checking purposes. *)
          val block_num = (case block of NONE => RPC.Blockchain.lastBlock () | SOME n => n)
          val meets_condition = TypeCoinCrypto.checkCondition block_num condition
          val () = if meets_condition then () else
                   raise TypeCoinError "condition does not hold"


          (* Ok. Everything checks out! Now we just need to update the
           * data structures. *)
          val basis'' = LogicCheck.installBasis basis txnid persistent_basis
          (* Fix up this references in the output props *)
          val output_props' = map (LogicSubst.replaceThisProp (Const.LId txnid)) output_props
          val tr' = TxnDict.insert tr txnid (Vector.fromList output_props')

      in
          (basis'', tr')
      end

  fun checkTransaction basis tr (block, txnid, [tx]) =
      (* If there is only one, don't catch the errors *)
      checkTransaction' basis tr (block, txnid, tx)
    | checkTransaction basis tr (block, txnid, tx :: txs) =
      (* I am really sloppy about exceptions... *)
      (checkTransaction' basis tr (block, txnid, tx)
       handle BlockExplorer.ExplorerError => raise BlockExplorer.ExplorerError
            | _ => checkTransaction basis tr (block, txnid, txs))
    | checkTransaction _ _ (_, _, []) =
      raise TypeCoinError "empty transaction"


  fun checkChain basis tr txns =
      foldl (fn (txn, (basis, tr)) => checkTransaction basis tr txn) (basis, tr) txns

  end

end


(*
(* Renames transactions in a typecoin transaction *)
(* Is missing an important part, so, welp. *)
structure TypeCoinRename =
struct
  local
    open Logic TypeCoinTxn
  in

  fun renameProp name' name prop =
      LogicSubst.replaceLocProp (Const.LId name') (Const.LId name) prop

  fun renameInput name' name (Input {source = (txnid, n), prop}) =
      (Input {source = (if txnid = name then name' else txnid, n),
              prop = renameProp name' name prop})
  (* wtb functional record update *)
  fun renameOutput name' name
      (Output {prop, dest, needs_receipt, amount}) =
      (Output {prop = renameProp name' name prop,
               dest = dest, needs_receipt = needs_receipt, amount = amount})

  fun renameAffirmation name' name {prop, persistent, principal, crypto_sig} =
      {prop = renameProp name' name prop,
       persistent = persistent, principal = principal, crypto_sig = crypto_sig}

  fun renameBasisEntry name' name entry =
      (case entry of
           SRule (i, prop) => SRule (i, renameProp name' name prop)
         | SConst (c, i, e) => SConst (c, i,
                                       LFSubst.substAndReplaceExp 0 [] 0
                                       (Const.LId name') (Const.LId name) e))

  fun renameProof _ = raise Fail "unimplemented. sigh"

  fun renameTxnBody name' name
      (TxnBody {inputs, basis, linear_grant, outputs, proof_term,
                name=txnname, metadata}) =
      TxnBody {inputs = map (renameInput name' name) inputs,
               outputs = map (renameOutput name' name) outputs,
               linear_grant = map (renameProp name' name) linear_grant,
               basis = map (renameBasisEntry name' name) basis,
               proof_term = renameProof proof_term,
               name = txnname,
               metadata = metadata}

  end
end
*)

