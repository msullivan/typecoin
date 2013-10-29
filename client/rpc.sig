
signature RPC =
   sig

      exception RPC
      exception RemoteError of string

      val close : unit -> unit

      val lastBlock : unit -> int
      val positionByNumber : int -> Int64.int
      val getTransactionByHash : Bytestring.string -> Transaction.tx option

      val inject : Transaction.tx -> unit

   end