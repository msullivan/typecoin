
structure Commo : COMMO =
   struct

      type peer = Protocol.bufsock * Time.time ref
      type comm = peer * Message.message

      (* MLton doesn't offer an event interface for sockets, so instead we wrap
         each socket with a service thread.
      *)


      fun recvLoop (peer as (bufsock, _)) ch =
         let
            val msg = Protocol.recvMessage bufsock
            (* Allow the thread to evaporate if this raises an exception. *)
         in
            CML.send (ch, (peer, msg));
            recvLoop peer ch
         end

      fun commoLoop inch outch =
         (
         (* XX much more to do here *)
         CML.send (outch, CML.recv inch);
         commoLoop inch outch
         )

      val theChannel : comm CML.chan option ref = ref NONE

      fun initialize () =
         let
           val inch = CML.channel ()
           val outch = CML.channel ()

           val () = theChannel := SOME inch
        in
           CML.spawn (fn () => commoLoop inch outch);
           outch
        end

     fun cleanup () = theChannel := NONE


     fun addPeer addr =
        (case Protocol.handshake addr of
            NONE => NONE
          | SOME bufsock =>
               (case !theChannel of
                   NONE =>
                      raise (Fail "communication manager not initialized")
                 | SOME ch =>
                      let
                         val peer = (bufsock, ref (Time.now ()))
                      in
                         CML.spawn (fn () => recvLoop peer ch);
                         SOME peer
                      end))

     fun sendMessage ((sock, _), _) msg =
        let
           val b =
              Network.sendVec (sock, Bytesubstring.full (Message.writeMessage msg))
              handle OS.SysErr _ => false
                   | exn => (
                            (* XX get rid of this *)
                            Platform.print ("unexpected exception " ^ exnName exn ^ "\n");
                            false
                            )
        in
           if b then
              ()
           else
              Network.close sock;
           b
        end

   end
