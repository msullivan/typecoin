
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

      type header =
         {
         version : int,
         previous : Bytestring.string,
         root : Bytestring.string,
         timestamp : Word32.word,
         bits : Word32.word,  (* encoded difficulty *)
         nonce : Word32.word
         }

      type block = header * Transaction.tx list

      exception InvalidBlock

      fun headerWriter ({version, previous, root, timestamp, bits, nonce}:header) =
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
            W.word32L bits
            >>>
            W.word32L nonce

      fun writer (header, txs) =
         headerWriter header
         >>>
         W.varint (length txs)
         >>>
         W.list Transaction.writer txs

      val headerReader =
         R.wrap Word32.toInt R.word32L
         >>= (fn version =>
         R.bytes 32
         >>= (fn previous =>
         R.bytes 32
         >>= (fn root =>
         R.word32L
         >>= (fn timestamp =>
         R.word32L
         >>= (fn bits =>
         R.word32L
         >>= (fn nonce =>
         R.return { version=version, previous=previous, root=root, timestamp=timestamp,
                    bits=bits, nonce=nonce }
         ))))))

      val reader =
         headerReader
         >>= (fn header =>
         R.varint
         >>= (fn count =>
         R.count count Transaction.reader
         >>= (fn txs =>
         R.return (header, txs)
         )))

      fun readBlock bytes = Reader.readfull reader bytes


      fun traverseBlock f accInitial blockstr =
         let
            val (_, blockstr') = Reader.readS headerReader (BS.full blockstr)
            val (count, txsstr) = Reader.readS Reader.varint blockstr'

            val pos = B.size blockstr - BS.size txsstr

            fun loop i pos str acc =
               if i >= count then
                  acc
               else
                  let
                     val (tx, str') = Reader.readS Transaction.reader str
                     val txsz = BS.size str - BS.size str'
                     val txstr = BS.slice (str, 0, SOME txsz)

                     val acc' = f (i, pos, tx, txstr, acc)
                  in
                     loop (i+1) (pos + txsz) str' acc'
                  end
         in
            loop 0 pos txsstr accInitial
         end

   end
