
signature VERIFIER =
   sig
      val computeUtxo : int -> Utxo.table
      val verifier : int -> int -> string -> unit
   end