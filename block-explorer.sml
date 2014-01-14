(* Code to try to figure out where a particular txout has been spent.
 * We do this by contacting blockexplorer.com and screenscraping the
 * result. This is pretty terrible.
 * Obviously blockexplorer.com shouldn't be trusted directly, and we
 * should double check the results.
 *)

structure BlockExplorer =
struct
  exception ExplorerError

  structure HTTP = CurlDownloader

  val isTestnet = false
  fun getBaseUrl () =
      if isTestnet then "http://blockexplorer.com/testnet/"
      else "http://blockexplorer.com/"

  fun eq x1 x2 = x1 = x2 (* woo currying *)

  fun getSpendingTx' txid outnum =
      (let val url = getBaseUrl () ^ "tx/" ^ txid
           val lines = HTTP.retrieveLines url
           val name = "name=\"o" ^ (Int.toString outnum) ^ "\""
           val line = valOf (List.find (String.isSubstring name) lines)
      (* I'm not proud of any of this. *)
       in
           if String.isSubstring "Not yet redeemed" line then NONE else
       let val part1 = List.nth (String.tokens (eq #"/") line, 2)
           val spending_tx = List.nth (String.tokens (eq #"#") part1, 0)
       in SOME spending_tx end end)
      handle _ => raise ExplorerError

  fun getTxBlock' txid =
      (let val url = getBaseUrl () ^ "tx/" ^ txid
           val lines = HTTP.retrieveLines url
           val line = valOf (List.find (String.isSubstring "Appeared in") lines)

           val part1 = List.nth (String.tokens (eq #" ") line, 4)
           val block = List.nth (String.tokens (eq #"<") part1, 0)
       in valOf (Int.fromString block) end)
      handle _ => raise ExplorerError

  fun getSpendingTx (txid, outnum) =
      Option.map TypeCoinTxn.fromHexId
      (getSpendingTx' (TypeCoinTxn.toHexId txid) outnum)

  fun getTxBlock txid = getTxBlock' (TypeCoinTxn.toHexId txid)

  fun getSpendingTxAndBlock coord =
      (case getSpendingTx coord of
           NONE => NONE
         | SOME tx => SOME (tx, getTxBlock tx))

end
