
structure Sink :> SINK =
   struct

      structure B = Bytestring
      structure BS = Bytesubstring

      datatype sink =
         DONE
       | MORE of int * (unit -> unit) * (Bytesubstring.substring -> sink)

      fun register sock sink =
         (case sink of
             DONE => ()
           | MORE (req, fk, sk) =>
                let
                   val haver = ref 0
                   val bufr : B.string list ref = ref []
                   val goalr = ref req
                   val fkr = ref fk
                   val skr = ref sk
                
                   fun service () =
                      let
                         fun loop have buf goal fk sk =
                            if have < goal then
                               (
                               haver := have;
                               bufr := buf;
                               goalr := goal;
                               fkr := fk;
                               skr := sk
                               )
                            else
                               let
                                  val str = B.concat (rev buf)
                               in
                                  (case sk (BS.substring (str, 0, goal)) of
                                      DONE =>
                                         (
                                         Scheduler.delete sock;
                                         Network.close sock
                                         )
                                    | MORE (req', fk', sk') =>
                                         loop (have-goal) [B.extract (str, goal, NONE)] req' fk' sk')
                                  (* Should we catch exceptions here? *)
                               end

                         val v =
                            Network.recvVec sock

                         val sz = B.size v
                      in
                         if sz = 0 then
                            (* Socket is closed or gone bad, exit. *)
                            (
                            Scheduler.delete sock;
                            Network.close sock;
                            fk ()
                            )
                         else
                            loop (!haver + sz) (v :: !bufr) (!goalr) (!fkr) (!skr)
                      end
                in
                   Scheduler.insertRead sock service
                end)

   end
