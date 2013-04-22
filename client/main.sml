
structure Main =
   struct

      (* constants *)
      val timeslice = Time.fromMilliseconds 20

      val log : Message.message list ref = ref []

      fun loop evt =
         (case CML.sync evt of
             NONE =>
                (
                Platform.print "\n";
                Platform.print (Int.toString (length (!log)));
                RunCML.shutdown OS.Process.success
                )
           | SOME (_, msg) =>
                (
                log := msg :: !log;
                loop evt
                ))

      fun main () =
         let
            val () = MLton.Signal.restart := false

            val ch = Commo.initialize ()

            val l = Network.dns "testnet-seed.bitcoin.petertodd.org"

            val () =
               List.app
               (fn addr =>
                   (case Commo.addPeer addr of
                       NONE => ()
                     | SOME peer =>
                          (
                          Commo.sendMessage peer Message.Getaddr;
                          ()
                          )))
               l

            val timeout = CML.atTimeEvt (Time.+ (Time.now (), Time.fromSeconds 25))
            val evt =
               CML.choose [CML.wrap (timeout, (fn () => NONE)),
                           CML.wrap (CML.recvEvt ch, SOME)]
         in
            loop evt
         end

      fun go () =
         RunCML.doit (main, SOME timeslice)

   end
