
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

(*
val block : Block.block =
   {
   version = 2,
   previous = bfh "82c7aad66f518fc33bc80e16d2b7c8e77f25a94bbcdd76e9827e950300000000",
   root = bfh "7235f506846862a118a8dd55fd3791c4c3e2149a1601b74ac1489624271bded6",
   timestamp = 0w1367090911,
   difficulty = 0w470159668,
   nonce = 0w1018098699,
   count = 1,
   transactions =
      [{
       inputs = [{
                 from = (bfh "0000000000000000000000000000000000000000000000000000000000000000", 0wxffffffff),
                 script = bfh "03200b010110062f503253482f",
                 sequence = 0wxffffffff
                 }],
       outputs = [{
                  amount = 5000000000,
                  script = bfh "210362fe4a63daeeaeb5645454099e253b12e6990cbbdaf11f1b935266a51cf33b96ac"
                  }],
       lockTime = 0w0
       }]
   }

val blh = Writer.write (Block.writeBlockHeader block)
val bl = Writer.write (Block.writeBlock block)
val t = hd (#transactions block)
val str = Writer.write (Transaction.writeTx t)
*)

(*
val block1 : Block.block =
   {
   version = 1,
   previous = Chain.genesisHash,
   root = bfh "0000000000000000000000000000000000000000000000000000000000000000",
   timestamp = 0w0,
   difficulty = 0w0,
   nonce = 0w0,
   count = 0,
   transactions = []
}

val hash1 = dhash (Writer.write (Block.writeBlockHeader block1))

val block2 : Block.block =
   {
   version = 1,
   previous = hash1,
   root = bfh "0000000000000000000000000000000000000000000000000000000000000000",
   timestamp = 0w0,
   difficulty = 0w0,
   nonce = 0w0,
   count = 0,
   transactions = []
}

val hash2 = dhash (Writer.write (Block.writeBlockHeader block2))

val block3 : Block.block =
   {
   version = 1,
   previous = hash2,
   root = bfh "0000000000000000000000000000000000000000000000000000000000000000",
   timestamp = 0w0,
   difficulty = 0w0,
   nonce = 0w0,
   count = 0,
   transactions = []
}

val hash3 = dhash (Writer.write (Block.writeBlockHeader block3))

val block1' : Block.block =
   {
   version = 1,
   previous = Chain.genesisHash,
   root = bfh "0000000000000000000000000000000000000000000000000000000000000000",
   timestamp = 0w0,
   difficulty = 0w0,
   nonce = 0w1,
   count = 0,
   transactions = []
}

val hash1' = dhash (Writer.write (Block.writeBlockHeader block1'))

val block2' : Block.block =
   {
   version = 1,
   previous = hash1',
   root = bfh "0000000000000000000000000000000000000000000000000000000000000000",
   timestamp = 0w0,
   difficulty = 0w0,
   nonce = 0w1,
   count = 0,
   transactions = []
}

val hash2' = dhash (Writer.write (Block.writeBlockHeader block2'))

val block3' : Block.block =
   {
   version = 1,
   previous = hash2',
   root = bfh "0000000000000000000000000000000000000000000000000000000000000000",
   timestamp = 0w0,
   difficulty = 0w0,
   nonce = 0w1,
   count = 0,
   transactions = []
}

val hash3' = dhash (Writer.write (Block.writeBlockHeader block3'))

val bl1 = Writer.write (Block.writeBlock block1)
val bl2 = Writer.write (Block.writeBlock block2)
val bl3 = Writer.write (Block.writeBlock block3)
val bl1' = Writer.write (Block.writeBlock block1')
val bl2' = Writer.write (Block.writeBlock block2')
val bl3' = Writer.write (Block.writeBlock block3')
;

BC.initialize ();
BC.insertBlock hash2 bl2;
BC.insertBlock hash1 bl1;
BC.insertBlock hash1' bl1';
BC.insertBlock hash2 bl2;
BC.insertBlock hash2' bl2';
BC.insertBlock hash3' bl3';
*)

(*
fun removedups' eq x l =
   (case l of
       [] => [x]
     | h :: t =>
          if eq (x, h) then
             removedups' eq x t
          else
             x :: removedups' eq h t)

fun removedups eq l =
   (case l of
       [] => []
     | h :: t =>
          removedups' eq h t)
*)


val block = {
            version=1,
            previous=bfh "0000000000000000000000000000000000000000000000000000000000000000",
            root = B.rev (bfh "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b"),
            timestamp = 0w1231006505,
            nonce = 0w2083236893,
            difficulty=0w486604799,
            count = 1,
            transactions =
               [{ inputs = [{ from=(bfh "0000000000000000000000000000000000000000000000000000000000000000", 0wxffffffff),
                              script = bfh "04ffff001d0104455468652054696d65732030332f4a616e2f32303039204368616e63656c6c6f72206f6e206272696e6b206f66207365636f6e64206261696c6f757420666f722062616e6b73",
                              sequence = 0wxffffffff }],
                  outputs = [ {amount=5000000000,
                               script = bfh "4104678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5fac"} ],
                  lockTime = 0w0 }]
            } : Block.block
