
signature SEED =
   sig

      val initialSeed : unit -> unit
      val writeSeedFile : unit -> unit
      val addTimeEntropy : unit -> unit

   end
