
structure Sink :> SINK =
   struct

      structure B = Bytestring
      structure BS = Bytesubstring

      datatype sink =
         DONE
       | MORE of int * (Bytesubstring.substring -> sink)

      fun register sock sink =
         (case sink of
             DONE => ()
           | MORE (req, f) =>
                let
                   val haver = ref 0
                   val bufr : B.string list ref = ref []
                   val goalr = ref req
                   val kr = ref f
                
                   fun service () =
                      let
                         fun loop have buf goal k =
                            if have < goal then
                               (
                               haver := have;
                               bufr := buf;
                               goalr := goal;
                               kr := k
                               )
                            else
                               let
                                  val str = B.concat (rev buf)
                               in
                                  (case k (BS.substring (str, 0, goal)) of
                                      DONE =>
                                         (
                                         Network.tryClose sock;
                                         Scheduler.delete sock
                                         )
                                    | MORE (req', f') =>
                                         loop (have-goal) [B.extract (str, goal, NONE)] req' f')
                                  (* Should we catch exceptions here? *)
                               end

                         val v =
                            Network.recvVec sock
                            handle OS.SysErr _ =>
                               (* Socket has gone bad, shut it down. *)
                               (
                               Network.tryClose sock;
                               Scheduler.delete sock;
                               Scheduler.yield ()
                               )

                         val sz = B.size v
                      in
                         if sz = 0 then
                            (* Socket is closed, exit. *)
                            (
                            Network.tryClose sock;
                            Scheduler.delete sock
                            )
                         else
                            loop (!haver + sz) (v :: !bufr) (!goalr) (!kr)
                      end
                in
                   Scheduler.insertRead sock service
                end)

   end
