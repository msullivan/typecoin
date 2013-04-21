
structure Reader : READER =
   struct

      structure B = Bytesubstring

      structure Parsing = ParsingFun (type token = Word8.word
                                      structure Streamable = BytesubstringMonoStreamable)

      open Parsing

      type 'a reader = 'a Parsing.parser


      fun byte str =
         (case B.getc str of
             NONE =>
                raise SyntaxError
           | SOME (b, str') =>
                (b, str'))

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

      fun word16L str =
         if B.size str < 2 then
            raise SyntaxError
         else
            let
               val w = 
                  Word.orb (Word.<< (ConvertWord.word8ToWord (B.sub (str, 1)), 0w8),
                            ConvertWord.word8ToWord (B.sub (str, 0)))
            in
               (w, B.slice (str, 2, NONE))
            end

      fun word32L str =
         if B.size str < 4 then
            raise SyntaxError
         else
            (ConvertWord.bytesToWord32SL (B.slice (str, 0, SOME 4)),
             B.slice (str, 4, NONE))

      fun word64L str =
         if B.size str < 8 then
            raise SyntaxError
         else
            (ConvertWord.bytesToWord64SL (B.slice (str, 0, SOME 8)),
             B.slice (str, 8, NONE))

      fun bytesS n str =
         if B.size str < n then
            raise SyntaxError
         else
            (B.slice (str, 0, SOME n),
             B.slice (str, n, NONE))

      fun all str = (str, B.null)


      fun bytes n str =
         let
            val (b, str') = bytesS n str
         in
            (B.string b, str')
         end

      val varint =
         bind byte
         (fn w =>
             if w < 0wxfd then
                return (Word8.toInt w)
             else if w = 0wxfd then
                wrap Word.toInt word16L
             else if w = 0wxfe then
                bind word32L
                (fn w' =>
                    if w' > 0wx3fffffff then
                       (* We allow sizes only up to 2^30. *)
                       raise SyntaxError
                    else
                       return (Word32.toInt w'))
             else
                raise SyntaxError)

      val bytesVar = bind varint bytes

      fun varlist r =
         bind varint
         (fn n => count n r)



      fun read f str = f str

      fun readfull f str =
         let
            val (x, str') = f str
         in
            if B.isEmpty str' then
               x
            else
               raise SyntaxError
         end

   end
