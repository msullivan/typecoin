
signature BLOCKCHAIN =
   sig

      type hash = Bytestring.string

      val insertBlock : hash -> Bytestring.string -> unit

      exception Absent
      val member : hash -> bool
      val blockPosition : hash -> Position.int     (* position in the record *)
      val blockNumber : hash -> int                (* block number in the chain *)
      val blockData : hash -> Bytestring.string    (* the block itself *)

      val lastBlock : unit -> int
      val hashByNumber : int -> hash
      val dataByNumber : int -> Bytestring.string

      val initialize : unit -> unit
      val close : unit -> unit

   end
