
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
  val lookupResource : resid -> {origin: BatchData.res_location, owner: userid, resource: resource}
  val getUserResources : userid -> resid list
  val lookupTransaction : batch_txnid -> TypeCoinTxn.txn_body

  (* Modification operations *)
  val insertTransaction : userid -> TypeCoinTxn.txn_body -> batch_txnid
  val insertResource : userid -> BatchData.res_location * resource -> resid
  val moveResource : resid -> BatchData.res_location -> unit
  val removeResource : resid -> unit

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
  fun serializeOrigin origin = IOTypes.writeToVector BatchData.writeRes_location origin
  fun deserializeOrigin v = valOf (IOTypes.readFromVector BatchData.readRes_location v)

  (* Lookup operations *)
  fun lookupResource id =
      let val {origin, owner, resource} = SQL.lookupResource id
      in {origin=deserializeOrigin origin, owner=owner, resource=deserializeProp resource} end
  val getUserResources = map #id o SQL.getUserResources
  val lookupTransaction = deserializeTxn o SQL.lookupTxn

  (* Modification operations *)
  fun insertTransaction user txn = SQL.insertTxn (serializeTxn txn)
  fun insertResource owner (origin, res) =
      SQL.insertResource
      {origin=serializeOrigin origin, owner=owner, resource=serializeProp res}
  fun moveResource resid origin =
      SQL.moveResource {origin=serializeOrigin origin, id=resid}
  val removeResource = SQL.removeResource

end

structure BatchStore = BatchStoreSql
