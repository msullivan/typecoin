
structure Seed :> SEED =
   struct

      fun fileExists filename =
         (OS.FileSys.fileSize filename; true)
         handle OS.SysErr _ => false
              | Overflow => true


      fun writeSeedFile () =
         let
            val path = OS.Path.concat (Constants.dataDirectory, Constants.seedFile ^ ".new")
            val path' = OS.Path.concat (Constants.dataDirectory, Constants.seedFile)
            val outs = BinIO.openOut path
         in
            BinIO.output (outs, AESFortuna.random 32);
            BinIO.closeOut outs;
            OS.FileSys.rename {old=path, new=path'};
            Log.long (fn () => "Seed file written")
         end
         handle OS.SysErr _ => Log.long (fn () => "Error writing seed file")


      fun initialSeed () =
         let
            val path = OS.Path.concat (Constants.dataDirectory, Constants.seedFile)
         in
            if fileExists path then
               let
                  val ins = BinIO.openIn path
               in
                  AESFortuna.addEntropy (0, BinIO.inputN (ins, 32));
                  BinIO.closeIn ins;
                  Log.long (fn () => "Seed file read")
               end
            else
               ();
               
            AESFortuna.addEntropy (0, ConvertIntInf.toBytesB (Time.toMilliseconds (Time.now ())))
         end
                  
            
      val currentPool = ref 0  (* <= AESFortuna.poolCount *)

      fun addTimeEntropy () =
         (
         AESFortuna.addEntropy (!currentPool, ConvertIntInf.toBytesB (Time.toMilliseconds (Time.now ())));
         currentPool := (!currentPool + 1) mod AESFortuna.poolCount
         )


   end
