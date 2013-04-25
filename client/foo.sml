
structure B = Bytestring
structure BS = Bytesubstring
structure M = Message
structure N = Network
structure S = Scheduler

val bth = Bytestring.toStringHex
val bfh = Option.valOf o Bytestring.fromStringHex

fun println str = (print str; print "\n")

(*
val addr = hd (Network.dns "testnet-seed.bitcoin.petertodd.org")
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
*)



(*
fun serve port sock =
   let
      fun action () =
         let
            val v = N.recvVec sock
         in
            print (Int.toString port);
            print ":";
            println (Byte.bytesToString v);
            if Word8Vector.length v = 0 then
               S.deleteSock sock
            else
               ()
         end
   in
      S.insertSock sock action
   end


fun awaitout port =
   let
      val addr = NetHostDB.addr (valOf (NetHostDB.getByName "127.0.0.1"))
      val sock = N.connect (addr, port)
   in
      serve port sock
   end


fun awaitin port =
   let
      val sport = N.listen port

      fun action () =
         let
            val sock = N.accept sport
         in
            serve port sock
         end
   in
      S.insertSock sport action
   end


fun timeout () = println "timeout"


fun go () =
   (
   awaitout 55555;
   awaitin 66666;
   S.setTimeout 3 timeout;
   S.start ()
   )

val () = go ()
*)


val l1 = List.filter (fn (_, {port, ...}) => port = 18333) l
val l2 = List.filter (fn (_, {address, ...}) => case address of M.V4 _ => true | _ => false) l1
val l3 = Mergesort.sort (fn ((x, _), (y, _)) => IntInfOrdered.compare (y, x)) l2
val l4 = map (fn (_, {address=M.V4 ad, ...}) => Network.implodeAddr ad) (List.take (l3, 100))
