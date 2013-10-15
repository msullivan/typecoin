
structure Reader : READER =
   struct

      structure B = Bytestring
      structure BS = Bytesubstring
      structure C = BytesubstringCostring

      structure Parsing = ParsingFun (type token = Word8.word
                                      structure Streamable = C.Streamable)

      open Parsing

      type 'a reader = 'a Parsing.parser


      fun byte str =
         (case C.getc str of
             NONE =>
                raise SyntaxError
           | SOME (b, str') =>
                (b, str'))

      fun word16B str =
         if C.minSize (str, 2) then
            let
               val w = 
                  Word.orb (Word.<< (ConvertWord.word8ToWord (C.sub (str, 0)), 0w8),
                            ConvertWord.word8ToWord (C.sub (str, 1)))
            in
               (w, C.suffix (str, 2))
            end
         else
            raise SyntaxError

      fun word16L str =
         if C.minSize (str, 2) then
            let
               val w = 
                  Word.orb (Word.<< (ConvertWord.word8ToWord (C.sub (str, 1)), 0w8),
                            ConvertWord.word8ToWord (C.sub (str, 0)))
            in
               (w, C.suffix (str, 2))
            end
         else
            raise SyntaxError

      fun word32L cos =
         if C.minSize (cos, 4) then
            let
               val (str, cos') = C.splitAt (cos, 4)
            in
               (ConvertWord.bytesToWord32SL str, cos')
            end
         else
            raise SyntaxError

      fun word64L cos =
         if C.minSize (cos, 8) then
            let
               val (str, cos') = C.splitAt (cos, 8)
            in
               (ConvertWord.bytesToWord64SL str, cos')
            end
         else
            raise SyntaxError

      fun bytesS n cos =
         if C.minSize (cos, n) then
            C.splitAt (cos, n)
         else
            raise SyntaxError

      fun all cos = (C.all cos, C.null)


      fun bytes n str =
         let
            val (b, str') = bytesS n str
         in
            (BS.string b, str')
         end

      fun fromDecoder d =
         (case d of
             Decoder.ANSWER x => return x
           | Decoder.INPUT f =>
                bind byte
                (fn b => fromDecoder (f b)))
                
      val varint = fromDecoder Decoder.varint
         
      val bytesVar = bind varint bytes

      fun varlist r =
         bind varint
         (fn n => count n r)


      fun readS f str =
         let
            val (x, cos) = f (C.full str)
         in
            (x, C.all cos)
         end
         (* Make sure we don't stop the program over an integer out of range. *)
         handle Overflow => raise SyntaxError

      fun read f str = readS f (BS.full str)

      fun readfullS f str =
         let
            val (x, cos) = f (C.full str)
         in
            if C.maxSize (cos, 0) then
               x
            else
               raise SyntaxError
         end
         handle Overflow => raise SyntaxError

      fun readfull f str = readfullS f (BS.full str)

   end
