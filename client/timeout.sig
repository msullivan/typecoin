
signature TIMEOUT =
   sig

      type tid

      val dummy : tid
      val onceAbs : Time.time -> (unit -> unit) -> tid
      val once : Time.time -> (unit -> unit) -> tid
      val repeating : Time.time -> (unit -> unit) -> tid
      val cancel : tid -> unit

      val initialize : unit -> unit

   end
