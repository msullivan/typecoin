
signature BLOCKCHAIN_RPC =
   sig
      type hash = Bytestring.string
      type pos = Int64.int

      val lastBlock : unit -> int
      val positionByNumber : int -> pos
      val getTx : hash -> Transaction.tx option
   end


signature PROCESS_RPC =
   sig
      val inject : Transaction.tx -> unit
   end


signature RPC =
   sig

      exception RPC
      exception RemoteError of string

      structure Blockchain : BLOCKCHAIN_RPC
      structure Process : PROCESS_RPC

   end
