
structure TypeCoinTxn =
struct

  type txnid = Const.namespace
  type crypto_sig = Logic.crypto_sig
  type crypto_address = Logic.crypto_address
  type crypto_principal = Logic.crypto_principal

  datatype input = Input of
           {source: txnid * int,
            prop: Logic.prop
           }
  type inputs = input list

  datatype output = Output of
           {dest: crypto_address,
            prop: Logic.prop
           }
  type outputs = output list

  type persistent_sg = Logic.sg_entry list

  datatype linear_sg_entry =
           LSResource of Logic.prop
         | LSSignedAffirmation of Logic.signed_affirmation
  type linear_sg = linear_sg_entry list


  datatype txn_body = TxnBody of
           {inputs: inputs,
            persistent_sg: persistent_sg,
            linear_sg: linear_sg,
            outputs: outputs,
            proof_term: Logic.proof}
  type txn = txnid * txn_body

  type chain = txn list

end
