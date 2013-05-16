
structure Constants =
   struct

      (*** Message constants ***)

      (* Send this protocol version number to peers. *)
      val protocolVersion = 70001

      (* Send this agent name to peers. *)
      val agentName = "/MLBTC:a0/"



      (*** Networking constants ***)

      (* Keep contacting peers until you have this many connections. *)
      val desiredConnections = 8

      (* Contact a new peer this often. *)
      val pollInterval = Time.fromSeconds 10               (* 10 seconds *)

      (* Ping all connections this often. *)
      val keepAliveInterval = Time.fromSeconds 45          (* 45 seconds *)

      (* When this long for a connection to go through. *)
      val connectTimeout = Time.fromSeconds 30             (* 30 seconds *)

      (* Connections become reapable when idle this long. *)
      val idleTimeout = Time.fromSeconds (10 * 60)         (* 10 minutes *)

      (* Reap reapable connections this often. *)
      val reapInterval = Time.fromSeconds (5 * 60)         (* 5 minutes *)

      (* After this many connection failures, draw the next peer to contact from the verified queue. *)
      val verifiedQueueThreshold = 4



      (*** Peer management constants ***)

      (* Update a peer's timestamp at most this often. *)
      val peerUpdateThreshold = Time.fromSeconds (5 * 60)   (* 5 minutes *)

      (* Use DNS if you don't know this many addresses. *)
      val minPeers = 40

      (* Keep at most this many addresses. *)
      val maxPeers = 1000

      (* Execute peer maintenance this often. *)
      val maintenanceInterval = Time.fromSeconds (10 * 60)  (* 10 minutes *)

      (* Throw out any peer this old. *)
      val tooOld = Time.fromSeconds (14 * 24 * 60 * 60)     (* 14 days *)

      (* Don't relay anything this old. *)
      val tooOldToRelay = Time.fromSeconds (3 * 60 * 60)    (* 3 hours *)

      (* Dock the timestamp of incoming relayed peers this much. *)
      val relayedPenalty = Time.fromSeconds (2 * 60 * 60)   (* 2 hours *)

      (* Give dns-acquired peers a timestamp this long before now.
         This must be greater than tooOldToRelay (so dns-acquired peers are
         not relayed), but less than tooOld (so we can still use them).
      *)
      val dnsPenalty = Time.fromSeconds (4 * 60 * 60)       (* 4 hours *)

      (* Accept at most this many addresses from one peer. *)
      val maxPeersFromSource = 40



      (*** Block download constants ***)

      (* Check sync throughput this often. *)
      val throughputInterval = Time.fromSeconds 30          (* 30 seconds *)    

      (* Want to receive at least this many bytes per throughputInterval. *)
      val syncThroughput = 3 * 1024 * 1024                  (* 3 MB *)

      (* Accept at most this many orphans from any one connection. *)
      val maxOrphans = 50

      (* Initialize each connection's orphan table to this size. *)
      val orphanTableSize = 7



      (*** Blockchain record constants ***)

      (* Allocate a block hash table of this size. *)
      val blockTableSize = 1024 * 1024                      (* 1 meg *)

      (* Allocate a block index of this size. *)
      val primaryForkSize = 512 * 1024                      (* 512 k *)

      (* Write a new index if processed this much additional data. *)
      val indexThreshold = 1024 * 1024                      (* 1 meg *)



      (*** Pool and relay constants ***)

      (* Allocate a pool of this size. *)
      val poolSize = 4096

      (* Relay inventory this often. *)
      val relayInterval = Time.fromSeconds 20

      (* Keep relayed transactions in the pool this long. *)
      val poolRetentionTime = Time.fromSeconds (10 * 60)    (* 10 minutes *)



      (*** Confirmation constants ***)

      (* Accept an invalid block with this many confirmations.
         (Also, consequently, only verify this many blocks at
         the end of the blockchain.)
      *)
      val chainTrustConfirmations = 10



      (* File constants *)

      val dataDirectory = "data"
      val seedFile = "seed.dat"
      val alertFile = "alert.dat"


      (* Randomization constants *)

      (* Write a random seed file this often. *)
      val writeSeedInterval = Time.fromSeconds (15 * 60)    (* 15 minutes *)

   end
