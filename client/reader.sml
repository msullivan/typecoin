
structure Reader : READER =
   struct

      structure B = Bytesubstring

      structure Parsing = ParsingFun (type token = Word8.word
                                      structure Streamable = BytesubstringMonoStreamable)

      open Parsing

      type 'a reader = 'a Parsing.parser


      fun sizemark str =
         if B.size str < 1 then
            raise SyntaxError
         else
            let
               val w = B.sub (str, 0)
            in
               if w < 0wxfd then
                  (Word8.toInt w, B.slice (str, 1, NONE))
               else
                  raise (Fail "large field unimplemented")
            end

      fun byte str =
         (case B.getc str of
             NONE =>
                raise SyntaxError
           | SOME (b, str') =>
                (b, str))

      fun word16B str =
         if B.size str < 2 then
            raise SyntaxError
         else
            let
               val w = 
                  Word.orb (Word.<< (ConvertWord.word8ToWord (B.sub (str, 0)), 0w8),
                            ConvertWord.word8ToWord (B.sub (str, 1)))
            in
               (w, B.slice (str, 2, NONE))
            end

      fun word32L str =
         if B.size str < 4 then
            raise SyntaxError
         else
            (ConvertWord.bytesToWord32L (B.string (B.slice (str, 0, SOME 4))),
             B.slice (str, 4, NONE))

      fun word64L str =
         if B.size str < 8 then
            raise SyntaxError
         else
            (ConvertWord.bytesToWord64L (B.string (B.slice (str, 0, SOME 8))),
             B.slice (str, 8, NONE))

      fun bytes n str =
         if B.size str < n then
            raise SyntaxError
         else
            (B.string (B.slice (str, 0, SOME n)),
             B.slice (str, n, NONE))


      val bytesVar = bind sizemark bytes


      fun read f str = f str

   end
