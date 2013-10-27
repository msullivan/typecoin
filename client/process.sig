
signature PROCESS =
   sig
   
      val initialize : unit -> unit
      val cleanup : unit -> unit
      val inject : Transaction.tx -> unit

   end
