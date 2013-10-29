
structure RPC :> RPC =
   struct

      structure B = Bytestring
      structure BS = Bytesubstring
      structure M = RpcMessage
      structure C = BytesubstringCostring


      val theSock : Network.asock option ref = ref NONE
      val theCostring : C.costring ref = ref C.null

      val loopback = Address.toInAddr (valOf (Address.fromString "127.0.0.1"))


      exception RPC
      exception RemoteError of string


      fun connect () =
         let
            val sock =
               Network.connect (loopback, Constants.rpcPort)
               handle Network.NetworkException _ => raise RPC
            
            fun receive () = SOME (BS.full (Network.recvVec sock))
         in
            theSock := SOME sock;
            theCostring := C.fromStream (Stream.fromProcess receive);
            sock
         end


      fun disconnect () =
         (
         Option.app Network.close (!theSock);
         theSock := NONE;
         theCostring := C.null
         )

         
      fun send sock str =
         (Network.sendVec (sock, BS.full (ConvertWord.word32ToBytesB (Word32.fromInt (B.size str))))
          andalso
          Network.sendVec (sock, BS.full str))
         handle Network.NetworkException _ => false


      fun trySend str =
         let
            val (sock, isfresh) =
               (case !theSock of
                   NONE =>
                      (connect (), true)
                 | SOME sock =>
                      (sock, false))
         in
            if send sock str then
               sock
            else
               (
               disconnect ();

               if isfresh then
                  raise RPC
               else
                  (* try once more if the connection was old *)
                  trySend str
               )
         end


      fun rpc req f =
         let
            val sock = trySend (Writer.write (M.requestWriter req))
         in
            let
               val cos = !theCostring

               val sz =
                  Word32.toInt (ConvertWord.bytesToWord32SB (C.slice (cos, 0, 4)))
                  handle Overflow => raise RPC
   
               val msg =
                  Reader.readfullS M.responseReader (C.slice (cos, 4, sz))
                  handle Reader.SyntaxError => raise RPC
            in
               theCostring := C.suffix (cos, 4+sz);
               
               (case msg of
                   M.Exception (M.String str) =>
                      raise (RemoteError (B.toString str))
                 | _ =>
                      f msg)
            end
            handle RPC =>
               (
               disconnect ();
               raise RPC
               )
         end


      fun close () =
         (case !theSock of
             NONE =>
                ()
           | SOME sock =>
                (
                send sock (Writer.write (M.requestWriter M.CloseChannel));
                disconnect ()
                ))


      fun lastBlock () =
         rpc M.LastBlock
         (fn M.Int i => i
           | _ => raise RPC)

      fun inject tx =
         rpc (M.Inject tx)
         (fn M.True => ()
           | _ => raise RPC)

      (* It would be better to get the position, then look it up in the blockchain record ourselves. *)
      fun getTransactionByHash hash =
         rpc (M.LookupTx hash)
         (fn M.Transaction tx => SOME tx
           | M.False => NONE
           | _ => raise RPC)
      
      fun positionByNumber i =
         rpc (M.PositionByNumber i)
         (fn M.Integer i => Int64.fromLarge i
           | _ => raise RPC)

   end
