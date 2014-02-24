
structure Int32Ordered
   :> ORDERED where type t = Int32.int
   =
   struct
      type t = Int32.int

      val eq : Int32.int * Int32.int -> bool = (op =)
      val compare = Int32.compare
   end

structure Int32SplayDict    = SplayDict (structure Key = Int32Ordered)
structure Int32SplaySet     = SplaySet (structure Elem = Int32Ordered)



structure Batch =
struct

local
    open TypeCoinTxn
    open BatchData

in

  val BatchError = Fail

  val batch_address = ref (Bytestring.fromString "DUNNO LOL")
  fun getMyAddress () = !batch_address

  fun setup address file = (batch_address := address; BatchStore.setup file)

  (* This needs vastly more checking. *)
  fun depositResource userid chain (coord as (txnid, idx)) =
      let
          (* check the chain provided *)
          val (basis, tr) = TypeCoinCheck.checkChain
                                LogicCheck.stdlib_basis
                                TxnDict.empty
                                chain

          (* TODO: check that the chain actually lives in the block chain *)

          (* look up the resource getting deposited *)
          val res = TypeCoinCheck.lookupTxout tr coord

          (* we require that the last transaction be the one being deposited *)
          val (block, txnid', [TxnBody {outputs, metadata, ...}]) = List.last chain
          val () = if txnid = txnid' then ()
                   else raise BatchError "deposit txnid doesn't match last txn in chain"

          val (Output {dest, ...}) = List.nth (outputs, idx)
          val () = if dest = getMyAddress () then ()
                   else raise BatchError "deposit destination isn't batch server"

          (* check that this is the intended recepient by checking the metadata *)
          val dest_metadata_str = "deposit-to=" ^ TypeCoinTxn.toHexId userid
          val () = if List.exists (fn x => x = dest_metadata_str) metadata then ()
                   else raise BatchError "txn not a deposit to proper account"

          val () = if not (BatchStore.isAlreadyDeposited coord) then ()
                   else raise BatchError "resource already deposited"

          (* TODO: probably store the chain somewhere. *)

          val id = BatchStore.insertResource userid (BatchData.RealTxout coord, res)
      in id end

  fun inputResid (Input {source=(fake_txnid, i), ...}) =
      (case (Int32.fromString fake_txnid, i) of
           (SOME resid, 0) => resid
         | _ => raise BatchError "invalid input resid")

  fun checkInput userid tr (input as Input {source=(fake_txnid, i), prop}) =
      let val resid = inputResid input

          val {origin, owner, resource, spent} = BatchStore.lookupResource resid
              handle _ => raise BatchError "couldn't lookup resid"
          val () = if not spent then ()
                   else raise BatchError "resource already spent"
          val () = if userid = owner then ()
                   else raise BatchError "attempting to spend someone else's resource"
          val () = LogicCheck.propEquality prop resource

      in TxnDict.insert tr fake_txnid (Vector.fromList [prop]) end

  fun checkInputs userid tr inputs =
      foldl (fn (input, tr) => checkInput userid tr input) tr inputs

  fun makeTransaction userid chain txn_body =
      let
          (* check the chain provided *)
          val (basis, tr) = TypeCoinCheck.checkChain
                                LogicCheck.stdlib_basis
                                TxnDict.empty
                                chain

          val (TxnBody {basis=provided_basis, linear_grant, inputs, outputs, ...}) = txn_body

          (* check some restrictions *)
          val () = if null provided_basis andalso null linear_grant then ()
                   else raise BatchError "batch txn can not contain basis or grant"

          (* check the inputs against the database and add them to the type record *)
          val tr' = checkInputs userid tr inputs

          val () = app (fn Output {needs_receipt, ...} =>
                           if needs_receipt then raise BatchError "receipts not supported"
                           else ())
                   outputs

          (* Actually check the transaction *)
          val (cond, _) = TypeCoinCheck.checkTransactionWithCond
                              basis tr' (NONE, "dummy", [txn_body])

          (* We could be more permissive in the conds we allow, but aren't yet. *)
          val () = if cond = Logic.CTrue then ()
                   else raise BatchError "txn uses complicated cond"

          (* Insert all of the stuff. *)
          fun submit_txn () =
              let val txnid = BatchStore.insertTransaction userid txn_body

                  (* N.B. that we can only get away with directly using the prop
                   * in the output because we disallow basises, so none of the outputs
                   * can reference self. *)
                  fun submit_output i (Output {dest, prop, ...}) =
                      BatchStore.insertResource
                          dest
                          (BatchData.BatchTxout (txnid, i), prop)

                  fun delete_input input = BatchStore.spendResource (inputResid input)

                  val () = app delete_input inputs
                  val resids = Util.mapi submit_output outputs
              in (txnid, resids) end

      in BatchStore.runTransactionally submit_txn end

  structure TxnidSet = Int32SplaySet

  (********* Doesn't really belong here ************)
  local
      val cnt = ref 0
  in
  fun new_var s = s ^ Int.toString (!cnt) before cnt := !cnt + 1
  end

  structure L = Logic

  (* This is a bit annoying with all the special cases *)

  fun build_tensor [] = L.POne
    | build_tensor [x] = x
    | build_tensor (x::xs) = L.PTensor (x, build_tensor xs)

  fun build_mtensor [] = L.MOne
    | build_mtensor [x] = x
    | build_mtensor (x::xs) = L.MTensor (x, build_mtensor xs)

  fun bind_tensor body e [] = body
    | bind_tensor body e [v] = L.MLet (e, v, body)
    | bind_tensor body e [v1, v2] = L.MTensorLet (e, v1, v2, body)
    | bind_tensor body e (v::vs) =
      let val v' = new_var "binding"
      in L.MTensorLet (e, v, v', bind_tensor body (L.MVar v') vs) end
  (*************************************************)




  fun residToVar id = "res" ^ Int32.toString id

  fun collect resid (stuff as (txns, real_inputs, seen)) =
      let val {origin, resource, spent, ...} = BatchStore.lookupResource resid
          val seen' = TxnidSet.insert seen resid
      in
      (case origin of

           RealTxout txout =>
           (txns, (resid, txout, resource) :: real_inputs, seen')
         | BatchTxout (txnid, i) =>
           if TxnidSet.member seen txnid then stuff else
           let
               val (txn as TxnBody {inputs, outputs, proof_term, ...}) =
                   BatchStore.lookupTransaction txnid

               val input_resids = map inputResid inputs
               val (txns', real_inputs', seen'') =
                   collectMany input_resids (txns, real_inputs, seen')

               val data = (txnid, inputs, outputs, proof_term)
           in
               (data :: txns', real_inputs', seen'')
           end

      )
      end
  and collectMany resids stuff =
      foldl (Util.uncurry2 collect) stuff resids

  fun findUnspent txns =
      List.concat (map (fn (id, _, _, _) => BatchStore.getUnspentTxnOutputs id) txns)


  fun batchTerm resid (txns, real_inputs, seen) unspent =
      let
          fun runFunction ((txnid, inputs, outputs, proof), body) =
              let val output_vars = map (residToVar o #id) (BatchStore.getTxnOutputs txnid)
                  val input_term = build_mtensor (map (L.MVar o residToVar o inputResid) inputs)
                  val output_var = new_var "output"

                  val rest = bind_tensor body (L.MVar output_var) output_vars

              in
                  L.MIfBind (L.MApp (proof, input_term),
                             output_var,
                             rest)
              end

          val output_stuff = build_mtensor (map (L.MVar o residToVar) unspent)

          val body = foldl runFunction (L.MIfReturn (L.CTrue, output_stuff)) txns
          val input_vars = map (residToVar o Util.first3) real_inputs
          val input_type = build_tensor (map Util.third3 real_inputs)
          val input_var = new_var "input"

          val term = L.MLam (input_var, input_type,
                             bind_tensor body (L.MVar input_var) input_vars)

      in term end

  (* TODO: we need to be able to actually create a bitcoin transaction.
   * For testing, we just make a bogus id *)
  fun createTxn txn_body =
      ((),
       (TypeCoinTxn.toHexId (TypeCoinCrypto.hash
                                 (IOTypes.writeToVector TypeCoinTxn.writeTxn_bodies [txn_body]))))
  fun submitTxn realTxn = ()


  fun withdrawResource userid resid =
      let
          val {owner, ...} = BatchStore.lookupResource resid
              handle _ => raise BatchError "couldn't lookup resid"
          val () = if userid = owner then ()
                   else raise BatchError "attempting to spend someone else's resource"



          val (stuff as (txns, real_inputs, seen)) =
              collect resid ([], [], TxnidSet.empty)
          (* If there aren't any txns, then we are just doing a
           * simple withdraw *)
          val unspent = if null txns then [resid]
                        else findUnspent txns

          val term = batchTerm resid stuff unspent

          val inputs = map (fn (_, source, prop) => Input {source=source, prop=prop}) real_inputs

          fun mkOutput resid' =
              let val dest = if resid' = resid then userid else getMyAddress ()
                  val prop = #resource (BatchStore.lookupResource resid')
              in Output { dest = dest, prop = prop,
                          needs_receipt = false, amount = NONE
                        }
              end
          val outputs = map mkOutput unspent

          val txn_body = TxnBody { name = "withdraw " ^ Int32.toString resid,

                                   inputs = inputs,
                                   outputs = outputs,
                                   proof_term = term,

                                   metadata = [],
                                   basis = [],
                                   linear_grant = []
                                 }

          val (real_txn, txnid) = createTxn txn_body

          fun updateOutput n resid' =
              if resid' = resid then BatchStore.spendResource resid'
              else
                  BatchStore.moveResource resid' (BatchData.RealTxout (txnid, n))

          fun doChanges () =
              (Util.appi updateOutput unspent;
               submitTxn real_txn)

          val () = BatchStore.runTransactionally doChanges


      in (txn_body, txnid) end

end
end
