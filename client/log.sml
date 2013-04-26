
structure Log :> LOG =
   struct

      fun log f = print (f ())

   end