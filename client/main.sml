
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

         Log.initialize "log";
         Blockchain.initialize ();

         (* start the scheduler and immediately call main' *)
         Scheduler.start main';
         (* done, cleaning up *)

         cleanup ()
         )

   end
