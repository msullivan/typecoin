

structure Batch =
struct

local
    open TypeCoinTxn
in

  val BatchError = Fail

  fun getMyAddress () = Bytestring.fromString "DUNNO LOL"

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

          val id = BatchStoreSql.insertResource userid (BatchData.RealTxout coord, res)
      in id end

end
end
