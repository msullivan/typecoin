
signature BLOCKCHAIN_RPC =
   sig
      type hash = Bytestring.string

      val member : hash -> bool
      val blockData : hash -> Bytestring.string
      val block : hash -> Block.block
      val blockPrimary : hash -> bool
      val lastBlock : unit -> int
      val totalDifficulty : unit -> IntInf.int
      val dataByNumber : int -> Bytestring.string
      val tx : hash -> Transaction.tx option
      val txDataByNumber : int -> int -> Bytestring.string
      val txByNumber : int -> int -> Transaction.tx
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
