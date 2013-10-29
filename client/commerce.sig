
signature COMMERCE =
   sig

      datatype output =
         (* Standard address :
            Spender must provide a public key that hashes to the given address,
            and a signature for that public key.
         *)
         PayToKeyHash of Bytestring.string

         (* Multisig m pubkeys :
            Spender must provide signatures for m of the public keys.
         *)
       | Multisig of int * Bytestring.string list

      exception Analyze
      exception Invalid of string
      
      val synthesize : output -> Script.inst list
      val analyze : Script.inst list -> output

      exception NoKey

      val createTx :
         (Bytestring.string -> Transaction.tx option)
         -> { inputs : Transaction.coord list,
              outputs : (output * LargeInt.int) list,
              fee : LargeInt.int,
              keys : ECDSAp.privkey list }
         -> Transaction.tx

   end
