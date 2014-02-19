structure T = Thread (structure T = ThreadBase
                      structure RC = SelectReactorCore
                      structure C = ConfigPrintEverything)

structure CV = CondVar(T)
structure CS = ChiralSocketFn(T)
structure SU = ChiralSockUtil(CS)
structure LR = LineReader(CS.Socket)


structure BatchServer =
struct
  val default_port = 5124

  fun batch_server (conn, conn_addr) () =
      let

          fun loop () =
              let val line = SU.recvVec (conn, 2048)
              in SU.sendVec (conn, line);
                 loop ()
              end

      in loop () end


  (* Adapted from some Stilts code *)
  fun spawn_server addr application =
      let
          val listener = CS.INetSock.TCP.socket ()

          val (server_host, server_port) = CS.INetSock.fromAddr addr
          val sbind = (NetHostDB.toString server_host, server_port)

          fun accept () = let
              val conn = CS.Socket.accept listener
              val t = T.new (application conn)
          in
              accept ()
          end

          fun app () = (
              CS.Socket.Ctl.setREUSEADDR (listener, true);
              CS.Socket.bind (listener, addr);
              CS.Socket.listen (listener, 9);
              accept ()
          ) handle x => (CS.Socket.close listener; raise x)

    in
      T.new app
    end


  fun main _ =
      let val () = print "Listening...\n"
          val serverthread = spawn_server (INetSock.any default_port) batch_server
      in
          T.run ();
          0
      end


end
