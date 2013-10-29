
signature LOG =
   sig
      val short : string -> unit
      val long : (unit -> string) -> unit

      val initialize : string -> unit
      val cleanup : unit -> unit
   end
