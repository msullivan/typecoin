
structure Main =
   struct

      fun midnight () =
         let
            val date = Date.fromTimeLocal (Time.now ())
         in
            Date.toTime
            (Date.date { year=Date.year date, month=Date.month date, day=Date.day date,
                         hour=0, minute=0, second=0, offset=NONE })
         end


(*
      fun doInject () =
         if Blockchain.lastBlock () < 266276 then
            (Scheduler.once (Time.fromSeconds 10) doInject; ())
         else
            let
               val bfh = valOf o Bytestring.fromStringHex
               val privkey : ECDSAp.privkey = 95245469629093752575860103020816037419470265044454156296322257695355402909823
               val tx =
                  Commerce.createTx
                  { inputs = [(Bytestring.rev (bfh "8752bcc9896c50eb441ba81511ffeb1d94a30f0804ce28de6c7ba43e5e45a88f"), 1)],
                    outputs = [(Commerce.PayToKeyHash (Textcode.decodeAddress "17CRAXF26sRxmTp9nuC8pQzumt7stCKuhe"), 40000)],
                    fee = 10000,
                    keys = [privkey] }
                  handle (exn as Commerce.Invalid error) =>
                     (
                     Log.long (fn () => "Invalid transaction composed: " ^ error);
                     raise exn
                     )
   
               fun wait () = 
                  if Commo.numberOfConnections () >= 2 then
                     Process.inject tx
                  else
                     (Scheduler.once (Time.fromSeconds 10) wait; ())
            in
               wait ()
            end
*)


      fun main' () =
         (
         Seed.initialSeed ();
         Seed.writeSeedFile ();
         Scheduler.repeating Constants.writeSeedInterval Seed.writeSeedFile;

         Scheduler.onceAbs (Time.+ (midnight (), Time.fromSeconds (60 * 60 * 24)))
         (fn () => (
                   Log.long (fn () => Date.toString (Date.fromTimeLocal (Time.now ())));
                   Scheduler.onceAbs (Time.+ (midnight (), Time.fromSeconds (60 * 60 * 24)));
                   ()
                   ));

         Scheduler.repeating (Time.fromSeconds 60)
         (fn () => Log.long (fn () => "Heartbeat: "
                                      ^ Int.toString (Commo.numberOfConnections ())
                                      ^ " connections"));

         Scheduler.repeating (Time.fromSeconds (60 * 60)) Blockchain.writeIndex;

         Process.initialize ();
         RpcServer.initialize ();

         ()
         )


      fun cleanup () =
         (
         RpcServer.cleanup ();
         Process.cleanup ();
         Blockchain.close ();
         Log.cleanup ()
         )


      fun main () =
         (
         (* Create data directory, if it doesn't exist. *)
         ((if OS.FileSys.isDir Constants.dataDirectory then
              ()
           else
              (
              Log.long (fn () => "Fatal error: cannot create data directory");
              raise (Fail "fatal error")
              ))
          handle OS.SysErr _ =>
             (OS.FileSys.mkDir Constants.dataDirectory
              handle OS.SysErr _ =>
                 (
                 Log.long (fn () => "Fatal error: cannot create data directory");
                 raise (Fail "fatal error")
                 )));

         Log.initialize ();
         Blockchain.initialize ();

         (* start the scheduler and immediately call main' *)
         Scheduler.start main';
         (* done, cleaning up *)

         cleanup ()
         )

   end
