
structure Log :> LOG =
   struct

      fun short str = print str

      fun long f =
         let
            val date = Date.fromTimeLocal (Time.now ())
         in
            print (f ());
            print " [";
            print (Int.toString (Date.hour date));
            print ":";
            if Date.minute date < 10 then
               print "0"
            else
               ();
            print (Int.toString (Date.minute date));
            print "]\n"
         end

   end