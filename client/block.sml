
structure Block :> BLOCK =
   struct

      structure W = Writer
      structure R = Reader
      structure B = Bytestring
      structure BS = Bytesubstring

      fun >>= (r, f) = R.bind r f
      fun >> (r, r') = R.seq r r'
      fun >>> (w, w') = W.seq w w'
      infixr 3 >>= >> >>>



      type word32 = Word32.word

      type block =
         {
         version : int,
         previous : Bytestring.string,
         root : Bytestring.string,
         timestamp : word32,
         difficulty : word32,
         nonce : word32,
         count : int,
         transactions : Transaction.tx list
         }

      exception InvalidBlock

      fun writeBlockHeader ({version, previous, root, timestamp, difficulty, nonce, ...}:block) =
         if B.size previous <> 32 orelse B.size root <> 32 then
            raise InvalidBlock
         else
            W.word32L (Word32.fromInt version)
            >>>
            W.bytes previous
            >>>
            W.bytes root
            >>>
            W.word32L timestamp
            >>>
            W.word32L difficulty
            >>>
            W.word32L nonce

      fun writeBlock (block as {version, previous, root, timestamp, difficulty, nonce, count, transactions}:block) =
         if length transactions <> count then
            raise InvalidBlock
         else
            writeBlockHeader block
            >>>
            W.varint count
            >>>
            W.list Transaction.writeTx transactions

      val readBlock =
         R.wrap Word32.toInt R.word32L
         >>= (fn version =>
         R.bytes 32
         >>= (fn previous =>
         R.bytes 32
         >>= (fn root =>
         R.word32L
         >>= (fn timestamp =>
         R.word32L
         >>= (fn difficulty =>
         R.word32L
         >>= (fn nonce =>
         R.varint
         >>= (fn count =>
         R.count count Transaction.readTx
         >>= (fn transactions =>
         R.return { version=version, previous=previous, root=root, timestamp=timestamp,
                    difficulty=difficulty, nonce=nonce, count=count, transactions=transactions }
         ))))))))

   end
