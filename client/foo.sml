
structure P = Protocol

fun println str = (print str; print "\n")

val addr = NetHostDB.addr (valOf (NetHostDB.getByName "testnet-seed.bitcoin.petertodd.org"))
val () = println (NetHostDB.toString addr)
val saddr = INetSock.toAddr (addr, 18333)

fun getip addr =
   let
      val l = String.fields (fn ch => ch = #".") (NetHostDB.toString addr)
   in
      if length l = 4 then
         map (Word8.fromInt o valOf o Int.fromString) l
      else
         raise (Fail "bad address string")
   end

val self = [0wx60,0wxEC,0wxE1,0wx53] : Word8.word list
val nonce = 0wxE7520FBEF1CA2BA0 : Word64.word


val msg =
   Writer.write
   (P.writeMessage
       (P.Version
           (P.mkVersion
               {
               self = P.mkNetaddr self,
               remote = P.mkNetaddr (getip addr),
               nonce = nonce,
               lastBlock = 0
               })))
   
fun logall (s, timeout) =
   let
      val sd = Socket.sockDesc s
      val t = Time.fromSeconds (timeout div 2)  (* SMLNJ bug *)

      fun loop acc =
         (case #rds (Socket.select {rds=[sd], wrs=[], exs=[], timeout=SOME t}) of
             [] =>
                (
                println "timeout";
                acc
                )
           | _ :: _ =>
                let
                   val str = Network.recv s
                in
                   if B.size str = 0 then
                      (
                      println "closed";
                      acc
                      )
                   else
                      (
                      println ".";
                      loop (str :: acc)
                      )
                end)
   in
      B.concat (rev (loop []))
   end


fun doit () =
   let
      val s = Network.connect (addr, 18333)
      val _ = Network.send (s, msg)
      val str = logall (s, 10)
   in
      Socket.close s;
      str
   end
