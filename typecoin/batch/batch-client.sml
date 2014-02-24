signature BATCH_CLIENT =
sig
  val authenticate : string -> int -> TypeCoinTxn.crypto_principal -> bool
  val depositResource : TypeCoinTxn.chain -> TypeCoinTxn.txnid * int ->
                        Int32.int
  val makeTransaction : TypeCoinTxn.chain -> TypeCoinTxn.txn_body ->
                        BatchStore.batch_txnid * BatchStore.resid list
  val withDrawResource : Int32.int -> TypeCoinTxn.txn_body * TypeCoinTxn.txnid
end


structure BatchClient :> BATCH_CLIENT =
struct
  val params : (Network.addr * int) option ref = ref NONE
  structure RpcClient = RpcClientFn(structure Params =
                                    struct
                                      fun getParams () = valOf (!params)
                                    end)


  open RpcClient
  open Unityped

  (* XXX code duplication from the btc client rpc code *)
  fun abort () =
      (
       close ();
       raise RPC
      )

  fun fromBool u =
      (case u of
           True => true
         | Nil => false
         | _ => abort ())

  fun fromInt u =
      (case u of
           Int i => i
         | _ => abort ())

  fun fromString u =
      (case u of
           String str => str
         | _ => abort ())

  fun fromBytestring u =
      (case u of
           Bytestring str => str
         | _ => abort ())

  fun fromCons f g u =
      (case u of
           Cons (x1, x2) => (f x1, g x2)
         | _ => abort ())

  fun fromList f u =
      (case u of
           Nil => []
         | Cons (x, xs) => f x :: fromList f xs
         | _ => abort ())


  val fromResid = Int32.fromInt o fromInt
  val fromBatchTxnId = Int32.fromInt o fromInt

  fun Resid id = Int (Int32.toInt id)

  fun encodeChain chain = Bytestring (IOTypes.writeToVector TypeCoinTxn.writeChain chain)
  fun encodeBody body = Bytestring (IOTypes.writeToVector TypeCoinTxn.writeTxn_body body)

  fun decodeBody body =
      (case IOTypes.readFromVector TypeCoinTxn.readTxn_body (fromBytestring body) of
           SOME x => x
         | _ => abort())


  (* Rpc calls *)
  fun authenticate address port userid =
      let (* Close any existing connection *)
          val () = close ()
          val address' = Address.toInAddr (valOf (Address.fromString address))
          val () = params := SOME (address', port)
      in fromBool (rpc (0w3, Bytestring userid)) end

  fun depositResource chain (txnid, idx) =
      fromResid (rpc (0w10, Cons (encodeChain chain,
                                  Cons (String txnid, Int idx))))

  fun makeTransaction chain body =
      fromCons
          fromBatchTxnId
          (fromList fromResid)
          (rpc (0w11, Cons (encodeChain chain, encodeBody body)))

  fun withDrawResource resid =
      fromCons decodeBody fromString
          (rpc (0w12, Resid resid))


end
