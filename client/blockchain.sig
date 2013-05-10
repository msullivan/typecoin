
signature BLOCKCHAIN =
   sig

      type hash = Bytestring.string

      type orphanage
      val newOrphanage : unit -> orphanage
      val orphanageMember : orphanage -> hash -> bool
      val orphanageSize : orphanage -> int

      datatype result = ORPHAN | NOEXTEND | EXTEND
      (* The block must be grossly-verified already. *)
      val insertBlock : orphanage -> EBlock.eblock -> result
      
      val suspendVerification : unit -> unit
      val resumeVerification : unit -> unit

      exception Absent
      val member : hash -> bool
      val blockPosition : hash -> Int64.int        (* position in the record *)
      val blockNumber : hash -> int                (* block number in the chain *)
      val blockData : hash -> Bytestring.string    (* the block itself *)

      val lastBlock : unit -> int
      val totalDifficulty : unit -> IntInf.int

      val hashByNumber : int -> hash
      val dataByNumber : int -> Bytestring.string

      val initialize : unit -> unit
      val writeIndex : unit -> unit
      val close : unit -> unit

   end
