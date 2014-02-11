

structure Batch =
struct

local
    open TypeCoinTxn

  fun mapi f l =
      let fun mapi' _ _ nil = nil
            | mapi' f n (x::xs) = (f n x)::mapi' f (n+1) xs
      in mapi' f 0 l end

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

          (* TODO: check that (tx, idx) hasn't been deposited before *)

          (* TODO: probably store the chain somewhere. *)

          val id = BatchStore.insertResource userid (BatchData.RealTxout coord, res)
      in id end

  fun checkInput userid tr (Input {source=(fake_txnid, i), prop}) =
      let val resid =
              (case (Int32.fromString fake_txnid, i) of
                   (SOME resid, 0) => resid
                 | _ => raise BatchError "invalid input resid")

          val {origin, owner, resource} = BatchStore.lookupResource resid
                                          handle _ => raise BatchError "couldn't lookup resid"
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

                  fun delete_input (Input {source=(fake_txnid, _), prop}) =
                      BatchStore.removeResource (valOf (Int32.fromString fake_txnid))

                  val () = app delete_input inputs
                  val resids = mapi submit_output outputs
              in (txnid, resids) end

      in BatchStore.runTransactionally submit_txn end


end
end
