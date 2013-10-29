
structure RpcServer :> RPC_SERVER =
   struct

      structure B = Bytestring
      structure BS = Bytesubstring
      structure M = RpcMessage


      val syntax = Writer.write (M.responseWriter M.Syntax)


      type conn =
         {
         sock : Network.asock,
         opn : bool ref,
         timeout : Scheduler.tid ref
         }


      fun closeConn ({ sock, opn, ... }:conn) =
         if !opn then
            (
            Scheduler.delete sock;
            Network.close sock;
            opn := false;
            Log.long (fn () => "RPC connection closed")
            )
         else
            ()


      fun recvMessage fk sk =
         let
            fun kmsg str =
               sk (Reader.readfullS M.requestReader str)
               handle Reader.SyntaxError => (fk (); Sink.DONE)

            fun k str =
               let
                  val sz = Word32.toInt (ConvertWord.bytesToWord32SB str)
               in
                  if sz > Constants.maximumRpcRequest then
                     (fk (); Sink.DONE)
                  else
                     Sink.MORE (sz, fk, kmsg)
               end
         in
            Sink.MORE (4, fk, k)
         end


      fun serve (conn as {sock, timeout, ...}:conn) =
         let
            fun fk () =
               (
               Network.sendVec (sock, BS.full (ConvertWord.word32ToBytesB (Word32.fromInt (B.size syntax))))
               andalso
               Network.sendVec (sock, BS.full syntax) ;
               
               closeConn conn
               )
         in
            recvMessage fk
            (fn req =>
                let
                   val () = Log.short "R"
                   val () = Scheduler.cancel (!timeout)

                   val resp =
                      (case req of
                          M.CloseChannel =>
                             (
                             closeConn conn;
                             Scheduler.exit ()
                             )
                        | M.ShutdownServer =>
                             Scheduler.shutdown ()
                        | _ =>
                             (RpcAction.act req
                              handle exn =>
                                 M.Exception (M.String (B.fromString (exnMessage exn)))))

                   val respstr = Writer.write (M.responseWriter resp)
                in
                   if
                      Network.sendVec (sock, BS.full (ConvertWord.word32ToBytesB (Word32.fromInt (B.size respstr))))
                      andalso
                      Network.sendVec (sock, BS.full respstr)
                   then
                      (
                      timeout := Scheduler.once Constants.rpcLifetime (fn () => closeConn conn);
                      serve conn
                      )
                   else
                      (
                      closeConn conn;
                      Sink.DONE
                      )
                end)
         end


      fun answer insock =
         let
            val (sock, iaddr, port) = Network.accept insock
            val timeout = ref Scheduler.dummy
            val conn = { sock=sock, opn=ref true, timeout=timeout }
         in
            Log.long (fn () => "RPC connection accepted");
            Sink.register sock (serve conn);
            timeout := Scheduler.once Constants.rpcLifetime (fn () => closeConn conn)
         end


      val theInsock : Network.psock option ref = ref NONE

      fun initialize () =
         let
            val insock = Network.listen Constants.rpcPort
         in
            theInsock := SOME insock;
            Scheduler.insertRead insock (fn () => answer insock)
         end
         handle _ =>
            (
            Log.long (fn () => "Unable to open socket for rpc server");
            Scheduler.shutdown ()
            )

      fun cleanup () =
         (
         Option.app Network.close (!theInsock);
         theInsock := NONE
         )

   end
