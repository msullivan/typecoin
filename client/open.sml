
val bth = Bytestring.toStringHex
val bfh = Option.valOf o Bytestring.fromStringHex
structure P = Protocol
structure M = Message
structure N = Network
structure B = Bytestring
structure BS = Bytesubstring

(*
val sha = SHA256.hashBytes
val ripemd = RIPEMD160.hashBytes
open EllipticCurveParams
structure Ep = ECDSAp
structure E2 = ECDSA2m
structure EDp = ECDERp
structure ED2 = ECDER2m
structure R = AESFortuna

fun timeit f =
   let
      val timer = Timer.startCPUTimer ()

      val x = f ()
   in
      (x, Timer.checkCPUTimes timer)
   end
*)

fun opn () = use "open.sml"
