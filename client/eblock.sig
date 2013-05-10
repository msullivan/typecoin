
(* Enhanced blocks, with memoization of useful information. *)

signature EBLOCK =
   sig

      type eblock

      val fromBytes : Bytestring.string -> eblock
      val toBytes : eblock -> Bytestring.string
      val toBlock : eblock -> Block.block

      val hash : eblock -> Bytestring.string
      val txhashes : eblock -> Bytestring.string list

   end
