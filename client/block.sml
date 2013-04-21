
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

      val readBlock =
         R.wrap Word32.toInt R.word32L
         >>= (fn version =>
         R.wrap B.rev (R.bytes 32)
         >>= (fn previous =>
         R.wrap B.rev (R.bytes 32)
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
