
structure Main =
   struct

      val log : Message.message list ref = ref []

      fun messageHdlr (peer, msg) =
         log := msg :: !log;



      val toContact : Network.addr list ref = ref []

      fun timeoutHdlr () =
         (case !toContact of
             [] =>
                Scheduler.setTimeout (Time.fromSeconds 5) Scheduler.shutdown
           | addr :: rest =>
                (
                print "adding peer\n";
                toContact := rest;
                Scheduler.setTimeout (Time.fromMilliseconds 30) timeoutHdlr;
                Commo.addPeer addr
                   (fn peer =>
                       (
                       print "handshake successful\n";
                       Commo.sendMessage peer Message.Getaddr;
                       ()
                       ))
                ))


      fun main () =
         let
            val () = Scheduler.cleanup ()
            val () = Scheduler.setTimeout (Time.fromSeconds 10) timeoutHdlr
            val () = Commo.initialize messageHdlr
            val () = toContact := Network.dns "testnet-seed.bitcoin.petertodd.org"
            
            val () = Scheduler.start ()

            val l =
               List.concat (map (fn Message.Addr l => l | _ => []) (!log))

            val (m, n) =
               foldl (fn (a, (m, n)) => (case #address (#2 a) of Message.V4 _ => (m+1, n) | Message.V6 _ => (m, n+1))) (0, 0) l
         in
            print (Int.toString m);
            print " V4s and ";
            print (Int.toString n);
            print " V6s\n"
         end

   end
