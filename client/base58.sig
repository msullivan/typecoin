
signature BASE58 =
   sig

      val encode : Bytestring.string -> string

      exception Invalid
      val decode : string -> Bytestring.string

   end
