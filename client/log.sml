
structure Log :> LOG =
   struct

      val theOutstream = ref TextIO.stdErr

      fun short str =
         (
         TextIO.output (!theOutstream, str);
         print str
         )

      fun long f =
         let
            val date = Date.fromTimeLocal (Time.now ())
            fun pr str =
               (
               print str;
               TextIO.output (!theOutstream, str)
               )
         in
            pr (f ());
            pr " [";
            pr (Int.toString (Date.hour date));
            pr ":";
            if Date.minute date < 10 then
               pr "0"
            else
               ();
            pr (Int.toString (Date.minute date));
            pr "]\n";
            TextIO.flushOut (!theOutstream)
         end

      fun fileExists filename =
         (OS.FileSys.fileSize filename; true)
         handle OS.SysErr _ => false

      fun initialize () =
         let
            val outs =
               (* This test shouldn't be necessary, but not every platform implements
                  openAppend according to spec.
               *)
               if fileExists "log" then
                  TextIO.openAppend "log"
               else
                  TextIO.openOut "log"
               
         in
            theOutstream := outs;
            long (fn () => Chain.name ^ " client");
            long (fn () => Date.toString (Date.fromTimeLocal (Time.now ())))
         end

      fun cleanup () =
         (
         TextIO.closeOut (!theOutstream);
         theOutstream := TextIO.stdErr
         )

   end