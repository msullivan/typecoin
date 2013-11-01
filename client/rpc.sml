
structure RPC :> RPC =
   struct

      open RpcClient
      open Unityped


      fun fromUnit u =
         (case u of
             Nil => ()
           | _ => raise RPC)

      fun fromBool u =
         (case u of
             True => true
           | Nil => false
           | _ => raise RPC)

      fun fromInt u =
         (case u of
             Int i => i
           | _ => raise RPC)

      fun fromInteger u =
         (case u of
             Integer i => i
           | _ => raise RPC)

      fun fromBytestring u =
         (case u of
             Bytestring str => str
           | _ => raise RPC)

      
      (* These method numbers and formats must be consistent with RpcAction. *)

      structure Blockchain =
         struct

            type hash = Bytestring.string
            type pos = Int64.int
      
            (* Must match the value in Blockchain. *)
            val blockOffsetInRecord : pos = 40

            fun member hash =
               fromBool (rpc (0w10, Bytestring hash))

            fun blockPosition hash =
               Int64.fromLarge (fromInteger (rpc (0w21, Bytestring hash)))

            fun blockNumber hash =
               fromInt (rpc (0w19, Bytestring hash))

            fun blockData hash =
               fromBytestring (rpc (0w11, Bytestring hash))

            fun block hash =
               Block.readBlock (blockData hash)

            fun blockPrimary hash =
               fromBool (rpc (0w12, Bytestring hash))

            fun lastBlock () =
               fromInt (rpc (0w13, Nil))

            fun totalDifficulty () =
               fromInteger (rpc (0w14, Nil))
      
            fun hashByNumber i =
               fromBytestring (rpc (0w20, Int i))

            fun dataByNumber i =
               fromBytestring (rpc (0w15, Int i))

            fun blockByNumber i =
               Block.readBlock (dataByNumber i)
      
            fun positionByNumber i =
               Int64.fromLarge (fromInteger (rpc (0w22, Int i)))

            fun tx hash =
               (case rpc (0w16, Bytestring hash) of
                   Bytestring txstr =>
                      (SOME (Transaction.readTx txstr)
                       handle Reader.SyntaxError => raise RPC)
                 | Nil => NONE
                 | _ => raise RPC)

            fun txDataByNumber i j =
               fromBytestring (rpc (0w17, Cons (Int i, Int j)))

            fun txByNumber i j =
               (Transaction.readTx (txDataByNumber i j)
                handle Reader.SyntaxError => raise RPC)

            fun txByPosition pos =
               (case rpc (0w23, Integer (Int64.toLarge pos)) of
                   Bytestring str =>
                      (Transaction.readTx str
                       handle Reader.SyntaxError => raise RPC)
                 | _ => raise RPC)

         end

      structure Process =
         struct
      
            fun inject tx =
               fromUnit (rpc (0w18, Bytestring (Transaction.writeTx tx)))


         end
   end
