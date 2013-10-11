
signature BLOCKCHAIN =
   sig

      type hash = Bytestring.string
      type pos = Int64.int

      type orphanage
      val newOrphanage : unit -> orphanage
      val orphanageMember : orphanage -> hash -> bool
      val orphanageSize : orphanage -> int

      datatype result = ORPHAN | REPEAT | NOEXTEND | EXTEND
      (* The block must be grossly-verified already. *)
      val insertBlock : orphanage -> EBlock.eblock -> result
      
      val suspendVerification : unit -> unit
      val resumeVerification : unit -> unit

      exception Absent
      val member : hash -> bool
      val blockPosition : hash -> pos              (* position in the record *)
      val blockNumber : hash -> int                (* block number in the chain *)
      val blockData : hash -> Bytestring.string    (* the block itself *)
      val blockPrimary : hash -> bool              (* is the block on the primary chain *)

      val lastBlock : unit -> int
      val totalDifficulty : unit -> IntInf.int

      val hashByNumber : int -> hash
      val dataByNumber : int -> Bytestring.string
      val positionByNumber : int -> pos

      val initialize : unit -> unit
      val writeIndex : unit -> unit
      val close : unit -> unit

   end
