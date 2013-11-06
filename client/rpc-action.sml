
structure RpcAction :> RPC_ACTION =
   struct

      open Unityped

      fun Bool true = True
        | Bool false = Nil

      fun Tx tx =
         Bytestring (Transaction.writeTx tx)

      fun act (method : Word8.word, args) =
         (* These method numbers and formats must be consistent with those in RPC.
            Method numbers less that 10 are reserved.
         *)
         (case (method, args) of
             (0w10, Bytestring hash) =>
                (* Blockchain.member *)
                Bool (Blockchain.member hash)

           | (0w11, Bytestring hash) =>
                (* Blockchain.blockData *)
                Bytestring (Blockchain.blockData hash)

           | (0w12, Bytestring hash) =>
                (* Blockchain.blockPrimary *)
                Bool (Blockchain.blockPrimary hash)

           | (0w13, Nil) =>
                (* Blockchain.lastBlock *)
                Int (Blockchain.lastBlock ())

           | (0w14, Nil) =>
                (* Blockchain.totalDifficulty *)
                Integer (Blockchain.totalDifficulty ())

           | (0w15, Int i) =>
                (* Blockchain.dataByNumber *)
                Bytestring (Blockchain.dataByNumber i)

           | (0w16, Bytestring hash) =>
                (* Blockchain.tx *)
                (case Blockchain.tx hash of
                    SOME tx =>
                       Tx tx
                  | NONE =>
                       Nil)

           | (0w17, Cons (Int i, Int j)) =>
                (* Blockchain.txDataByNumber *)
                Bytestring (Blockchain.txDataByNumber i j)

           | (0w18, Bytestring txstr) =>
                (* Process.inject *)
                ((Process.inject (Transaction.readTx txstr); Nil)
                 handle Reader.SyntaxError => Method)

           | (0w19, Bytestring hash) =>
                (* Blockchain.blockNumber *)
                Int (Blockchain.blockNumber hash)

           | (0w20, Int i) =>
                (* Blockchain.hashByNumber *)
                Bytestring (Blockchain.hashByNumber i)

           | (0w21, Bytestring hash) =>
                (* Blockchain.blockPosition *)
                Integer (Int64.toLarge (Blockchain.blockPosition hash))

           | (0w22, Int i) =>
                (* Blockchain.positionByNumber *)
                Integer (Int64.toLarge (Blockchain.positionByNumber i))

           | (0w23, Integer pos) =>
                (* Blockchain.txByPosition *)
                Tx (Blockchain.txByPosition (Int64.fromLarge pos))

           | (0w24, Int i) =>
                (* Blockchain.sizeByNumber *)
                Int (Blockchain.sizeByNumber i)

           | _ =>
                Method)

   end

