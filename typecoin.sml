

structure TypeCoinCheck =
struct
  local
      open TypeCoinTxn Logic

  in

  exception TypeCoinError of string

  type type_record = Logic.prop vector TxnDict.dict

  fun build_tensor [] = POne
    | build_tensor [x] = x
    | build_tensor (x::xs) = PTensor (x, build_tensor xs)

  fun checkInput tr (Input {source=(source_txn, idx), prop, ...}) =
      ((case TxnDict.find tr source_txn of
            NONE => (* Not a typecoin txn. Type must be 1 *)
            LogicCheck.propEquality prop POne
          | SOME source_outputs =>
            LogicCheck.propEquality prop (Vector.sub (source_outputs, idx)));
       prop)

  fun checkInputs tr inputs = map (checkInput tr) inputs

  fun checkOutput sg (Output {dest, prop, needs_receipt, ...}) =
      let val () = LogicCheck.checkProp sg LogicContext.empty prop
          val lf_hash = TypeCoinBasis.hashBytestringToHashObj dest
          val receipt =
              if needs_receipt then
                  SOME (PReceipt (TypeCoinBasis.address_hash lf_hash,
                                  prop))
              else NONE
      in (prop, receipt) end

  fun checkOutputs sg outputs = ListPair.unzip (map (checkOutput sg) outputs)

  fun checkLinearSigEntry sg (LSResource prop) =
      (LogicCheck.checkProp sg LogicContext.empty prop;
       LogicCheck.thawedProp prop;
       prop)
    | checkLinearSigEntry sg (LSSignedAffirmation affirm) =
      let val prop' = LogicCheck.affirmationToProp affirm
          val () = LogicCheck.checkProp sg LogicContext.empty prop'
          (* crypto is checked in checkCrypto *)
      in prop' end

  fun checkLinearSig sg linear_sg = map (checkLinearSigEntry sg) linear_sg


  fun checkCrypto (TxnBody {inputs, persistent_sg, linear_sg, ...}) =
      let val txnId = TypeCoinCrypto.buildTxnIdentifier inputs
          fun checkAffirmation affirmation =
              if TypeCoinCrypto.checkAffirmation txnId affirmation then ()
              else raise TypeCoinError "affirmation signature failure"
          fun checkSgEntry (SSignedAffirmation (_, affirm)) =
              checkAffirmation affirm
            | checkSgEntry _ = ()
          fun checkLinearSgEntry (LSSignedAffirmation affirm) =
              checkAffirmation affirm
            | checkLinearSgEntry _ = ()

          val () = app checkSgEntry persistent_sg
          val () = app checkLinearSgEntry linear_sg
      in () end


  fun checkTransaction sg tr
                       (txnid, TxnBody {inputs, persistent_sg, linear_sg, outputs, proof_term}) =
      let (* Check the inputs and the outputs and the signatures and build up
           * the data structures we need to check the proof term. *)
          val input_props = checkInputs tr inputs
          val sg' = LogicCheck.checkSignature sg persistent_sg
          val linear_sg_props = checkLinearSig sg' linear_sg
          val (output_props, receipt_props) = checkOutputs sg' outputs
          val receipt_props = List.mapPartial (fn x => x) receipt_props

          (* Build up the prop that we need to prove. *)
          val input_prop = build_tensor (input_props @ linear_sg_props @ receipt_props)
          val output_prop = build_tensor output_props
          val expected_prop = PLolli (input_prop, output_prop)

          (* Moment of truth: check the proof term. *)
          val actual_prop = LogicCheck.inferProofOuter sg' proof_term
          val () = LogicCheck.propEquality actual_prop expected_prop

          (* Ok. Everything checks out! Now we just need to update the
           * data structures. *)
          val sg'' = LogicCheck.installSignature sg txnid persistent_sg
          (* Fix up this references in the output props *)
          val output_props' = map (LogicSubst.replaceThisProp (Const.LId txnid)) output_props
          val tr' = TxnDict.insert tr txnid (Vector.fromList output_props')

      in
          (sg'', tr')
      end

  fun checkChain sg tr txns =
      foldl (fn (txn, (sg, tr)) => checkTransaction sg tr txn) (sg, tr) txns

  end

end
