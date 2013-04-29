
signature PROCESS =
   sig
   
      val log : Message.message list ref
      val initialize : unit -> unit

   end
