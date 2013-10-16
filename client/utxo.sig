
signature UTXO =
   sig

      type hash = Bytestring.string
      type pos = Int64.int

      val reset : unit -> unit

      (* process isValid pos blockstr
       
         1. blockstr must parse as a block.
         2. blockstr appears in the record at position pos

         If isValid then also:

         3. No transaction in the block may have the same
            hash as a transaction already in the table.
         4. Every txin in the block is a valid, unspent txout.
      *)
      val process : bool -> pos -> Bytestring.string -> unit

      exception Undo
      (* The input bytestring must be the most recent block to be processed (and not undone).

         Raises Undo if the undo buffer is exhausted.
      *)
      val undo : Bytestring.string -> unit

      exception TxoInvalid

      (* Raises TxoInvalid if the txout is invalid or already spent.
         Otherwise returns the transaction's position in the blockchain.
      *)
      val lookup : Transaction.coord -> pos

      val size : unit -> int

      (* The input is the position in the blockchain record, up to which the table accounts for.
         It's included in the table so that we can make sure it is consistent with the index.
      *)
      val writeTable : Int64.int -> unit  
      val readTable : Int64.int -> bool  (* true if success *)

   end
