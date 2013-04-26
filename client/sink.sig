
signature SINK =
   sig

      (* A sink is an abstraction for a consumer of transmissions.  A sink
         is either done (in which case the socket can be closed), or
         it is looking for a certain number of bytes.  Once it receives those
         bytes, a function is invoked on the full input.  That function
         processes the input, doing whatever work is appropriate, then returns
         a new sink to continue consuming transmissions.

         The register function attaches a sink to a socket.  More than one
         sink should not be attached to a single socket.
      *)

      datatype sink =
         DONE
       | MORE of int * (Bytesubstring.substring -> sink)

      val register : Network.asock -> sink -> unit

   end
