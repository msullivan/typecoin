
structure B = Bytestring
structure BS = Bytesubstring
structure M = Message
structure P = Protocol

val bth = Bytestring.toStringHex
val bfh = Option.valOf o Bytestring.fromStringHex
fun println str = (print str; print "\n")

val addr = hd (Network.dns "testnet-seed.bitcoin.petertodd.org")
(*
val addr = hd (Network.dns "173.208.219.162")
*)
val () = println (NetHostDB.toString addr)
val self = map Word8.fromInt [96, 236, 225, 83]


fun f addr =
   let
      val ss as (sock, _) = valOf (P.connect addr)
      val _ = Network.sendVec (sock, BS.full (M.writeMessage M.Getaddr))
      val x = P.recvMessage ss
   in
      P.close ss;
      SOME (addr, x)
   end handle NoMessage => NONE

fun go () =
   map f (Network.dns "testnet-seed.bitcoin.petertodd.org")