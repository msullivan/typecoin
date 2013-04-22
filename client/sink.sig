
signature SINK =
   sig

      datatype sink =
         DONE
       | MORE of int * (Bytesubstring.substring -> sink)

      val register : Network.asock -> sink -> unit

   end
