
signature BLOCKCHAIN =
   sig

      type hash = Bytestring.string

      type orphanage
      val newOrphanage : unit -> orphanage
      val orphanageMember : orphanage -> hash -> bool
      val orphanageSize : orphanage -> int

      datatype result = ORPHAN | NOEXTEND | EXTEND
      val insertBlock : orphanage -> hash -> Bytestring.string -> result

      exception Absent
      val member : hash -> bool
      val blockPosition : hash -> Position.int     (* position in the record *)
      val blockNumber : hash -> int                (* block number in the chain *)
      val blockData : hash -> Bytestring.string    (* the block itself *)

      val lastBlock : unit -> int
      val lastHash : unit -> hash
      val hashByNumber : int -> hash
      val dataByNumber : int -> Bytestring.string

      val initialize : unit -> unit
      val close : unit -> unit

   end
