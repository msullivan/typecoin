
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
         bits : word32,
         nonce : word32,
         count : int,
         transactions : Transaction.tx list
         }

      exception InvalidBlock

      fun writeBlockHeader ({version, previous, root, timestamp, bits, nonce, ...}:block) =
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

      fun writeBlock (block as {count, transactions, ...}:block) =
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
         >>= (fn bits =>
         R.word32L
         >>= (fn nonce =>
         R.varint
         >>= (fn count =>
         R.count count Transaction.readTx
         >>= (fn transactions =>
         R.return { version=version, previous=previous, root=root, timestamp=timestamp,
                    bits=bits, nonce=nonce, count=count, transactions=transactions }
         ))))))))

      fun hashBlockHeader str =
         SHA256.hashBytes (SHA256.hash (Stream.fromTable Bytesubstring.sub (BS.substring (str, 0, 80)) 0))



      val dhash = SHA256.hashBytes o SHA256.hashBytes
      fun dhash2 str1 str2 = SHA256.hashBytes (SHA256.hashBytes (B.^ (str1, str2)))

      fun merkleRoot n l =
         let
            fun double f l =
               let
                  val (h1, l') = f l
               in
                  (case l' of
                      nil =>
                         (* out of elements, duplicate h1 *)
                         (dhash2 h1 h1, nil)
                    | _ :: _ =>
                         let
                            val (h2, l'') = f l'
                         in
                            (dhash2 h1 h2, l'')
                         end)
               end
      
            fun loop n i f =
               if i >= n then
                  f
               else
                  loop n (i*2) (double f)
         in
            #1 (loop n 1 
                (fn [] => raise (Fail "no elements")
                  | h :: t => (dhash (Writer.write (Transaction.writeTx h)), t))
                l)
         end

   end
