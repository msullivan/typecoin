
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


(*
val l1 = List.filter (fn (_, {port, ...}) => port = 18333) l
val l2 = List.filter (fn (_, {address, ...}) => case address of M.V4 _ => true | _ => false) l1
val l3 = Mergesort.sort (fn ((x, _), (y, _)) => IntInfOrdered.compare (y, x)) l2
val l4 = map (fn (_, {address=M.V4 ad, ...}) => Network.implodeAddr ad) (List.take (l3, 100))
*)

val block : Block.block =
   {
   version = 1,
   previous = bfh "0000000000000000000000000000000000000000000000000000000000000000",
   root = bfh "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b",
   timestamp = 0w1296688602,
   difficulty = 0wx1D00FFFF,
   nonce = 0wx18AEA41A,
   count = 1,
   transactions =
      [{
       inputs = [{
                 from = (bfh "0000000000000000000000000000000000000000000000000000000000000000", 0wxffffffff),
                 script = bfh "04ffff001d0104455468652054696d65732030332f4a616e2f32303039204368616e63656c6c6f72206f6e206272696e6b206f66207365636f6e64206261696c6f757420666f722062616e6b73",
                 sequence = 0wxffffffff
                 }],
       outputs = [{
                  amount = 5000000000,
                  script = bfh "4104678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5fac"
                  }],
       lockTime = 0w0
       }]
   }

val bl = Writer.write (Block.writeBlock block)
