
signature LOG =
   sig
      val short : string -> unit
      val long : (unit -> string) -> unit
   end
