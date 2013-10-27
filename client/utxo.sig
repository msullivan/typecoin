
signature UTXO =
   sig

      type hash = Bytestring.string
      type pos = Int64.int
      type table


      (* Any operation taking a table can raise Expired if the table is too old. *)
      exception Expired


      val new : unit -> table
      val branch : table -> table
      val null : table


      (* unspent table blockNumber coord

         Returns false if the txout at coord is invalid, already spent, or too recent to spend in the indicated blockNumber.
      *)
      val unspent : table -> int -> Transaction.coord -> bool


      (* spend table blockNumber coord

         Marks the txout at coord as spent (if it's valid).
         Returns false if the txout at coord is invalid, already spent, or too recent to spend in the indicated blockNumber.
      *)
      val spend : table -> int -> Transaction.coord -> bool


      (* insert table hash pos numOutputs

         Insert an entry with position pos and numOutputs outputs into the table.
      *)
      val insert : table -> hash -> pos -> int -> unit


      (* insertCoinbase table hash pos numOutputs blockNumber

         Insert a coinbase entry with position pos, numOutputs outputs, and block number
         blockNumber into the table.
      *)
      val insertCoinbase : table -> hash -> pos -> int -> int -> unit


      (* processBlock table pos blockNumber blockstr
       
         1. blockstr must parse as a block.
         2. blockstr appears in the record at position pos

         Processes all of blockstr's txins, ignoring errors.
      *)
      val processBlock : table -> pos -> int -> Bytestring.string -> unit


      val find : table -> hash -> pos option

      val writeTables : BinIO.outstream -> table -> unit

      (* Returns a chronology of tables, starting with the most recent. *)
      val readTables : BinIO.instream -> table list option

   end
