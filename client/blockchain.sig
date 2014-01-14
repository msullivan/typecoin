
signature BLOCKCHAIN =
   sig

      type hash = Bytestring.string
      type pos = Int64.int

      val blockOffsetInRecord : pos

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
      val blockData : hash -> Bytestring.string    (* the block data *)
      val block : hash -> Block.block              (* the block itself *)
      val blockPrimary : hash -> bool              (* is the block on the primary chain *)
      val blockUtxo : hash -> Utxo.table           (* UTXO table incorporating the block *)

      val lastBlock : unit -> int
      val totalDifficulty : unit -> IntInf.int     (* difficulty on startup taken to be zero *)
      val currentUtxo : unit -> Utxo.table

      val hashByNumber : int -> hash
      val dataByNumber : int -> Bytestring.string
      val blockByNumber : int -> Block.block
      val positionByNumber : int -> pos
      val sizeByNumber : int -> int
      val utxoByNumber : int -> Utxo.table

      val tx : hash -> Transaction.tx option
      val txDataByNumber : int -> int -> Bytestring.string
      val txByNumber : int -> int -> Transaction.tx
      val txByPosition : pos -> Transaction.tx
      val txDataByNumberAndHash : int -> hash -> Bytestring.string
      val txByNumberAndHash : int -> hash -> Transaction.tx
      val txIndexByNumberAndHash : int -> hash -> int

      val isUnspent : Transaction.coord -> bool

      (* Starts with verification suspended. *)
      val initialize : unit -> unit

      val writeIndex : unit -> unit
      val close : unit -> unit

      val neverVerify : bool ref

   end
