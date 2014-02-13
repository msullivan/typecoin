
signature BATCH_STORE =
sig
  type userid
  type resid
  type batch_txnid
  type resource = Logic.prop

  val setup : string -> unit

  (* Transaction support *)
  val checkpoint : unit -> unit
  val commit : unit -> unit
  val rollback : unit -> unit
  val runTransactionally : (unit -> 'a) -> 'a

  (* Lookup operations *)
  val lookupResource : resid -> {origin: BatchData.res_location, owner: userid,
                                 resource: resource, spent: bool}
  val getUserResources : userid -> resid list
  val lookupTransaction : batch_txnid -> TypeCoinTxn.txn_body
  val getTxnOutputs : batch_txnid -> {id: resid, idx: int} list
  val getUnspentTxnOutputs : batch_txnid -> resid list

  (* Modification operations *)
  val insertTransaction : userid -> TypeCoinTxn.txn_body -> batch_txnid
  val insertResource : userid -> BatchData.res_location * resource -> resid
  val moveResource : resid -> BatchData.res_location -> unit
  val spendResource : resid -> unit

end


structure BatchStoreSql : BATCH_STORE =
struct
  type userid = TypeCoinTxn.crypto_address
  type resid = Int32.int
  type batch_txnid = Int32.int
  type resource = Logic.prop

  fun setup file = SQL.prepare (SQLite.opendb file)

  (* Transaction support *)
  val checkpoint = SQL.beginSqlTransaction
  val commit = SQL.commitSqlTransaction
  val rollback = SQL.rollbackSqlTransaction

  fun runTransactionally f =
      let val () = checkpoint ()
          val result = f ()
                       handle e => (rollback (); raise e)
          val () = commit ()
      in result end


  fun serializeTxn body = IOTypes.writeToVector TypeCoinTxn.writeTxn_body body
  fun deserializeTxn v = valOf (IOTypes.readFromVector TypeCoinTxn.readTxn_body v)
  fun serializeProp prop = IOTypes.writeToVector Logic.writeProp prop
  fun deserializeProp v = valOf (IOTypes.readFromVector Logic.readProp v)

  (* Lookup operations *)
  fun lookupResource id =
      let val {real_txn_origin, batch_txn_origin, index, owner, resource, spent} =
              SQL.lookupResource id
          val i = Int32.toInt index
          val origin =
              (case (real_txn_origin, batch_txn_origin) of
                   (SOME id, NONE) => BatchData.RealTxout (id, i)
                 | (NONE, SOME id) => BatchData.BatchTxout (id, i)
                 | _ => raise Fail "inconsistent database state")
      in {origin=origin, owner=owner, resource=deserializeProp resource, spent=spent <> 0} end

  val getUserResources = map #id o SQL.getUserResources
  val lookupTransaction = deserializeTxn o SQL.lookupTxn

  val getTxnOutputs = map (fn {resid, idx} => {id=resid, idx=Int32.toInt idx}) o SQL.getTxnOutputs
  val getUnspentTxnOutputs = map #id o SQL.getUnspentTxnOutputs

  fun formatResource origin =
      (case origin of
           BatchData.RealTxout (id, i) => (SOME id, NONE, Int32.fromInt i)
         | BatchData.BatchTxout (id, i) => (NONE, SOME id, Int32.fromInt i))

  (* Modification operations *)
  fun insertTransaction user txn = SQL.insertTxn (serializeTxn txn)
  fun insertResource owner (origin, res) =
      let val (real, batch, index) = formatResource origin
      in
          SQL.insertResource
              {real_txn_origin=real, batch_txn_origin=batch, index=index,
               owner=owner, resource=serializeProp res,
               debug_name=SOME (PrettyLogic.prettyProp res)}
      end

  fun moveResource resid origin =
      let val (real, batch, index) = formatResource origin
      in
          SQL.moveResource
              {real_txn_origin=real, batch_txn_origin=batch, index=index,
               id=resid}
      end
  val spendResource = SQL.spendResource

end

structure BatchStore = BatchStoreSql
