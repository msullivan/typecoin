
functor VarintFun (M : sig
                          include MONAD
                          exception SyntaxError
                          val byte : Word8.word m
                          val word16L : Word.word m
                          val word32L : Word32.word m
                       end)
   :>
   sig
      val varint : int M.m
   end
   =
   struct
      open M

      val varint =
         bind byte
         (fn w =>
             if w < 0wxfd then
                return (Word8.toInt w)
             else if w = 0wxfd then
                bind word16L (fn x => return (Word.toInt x))
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

   end


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

      structure Varint =
         VarintFun
         (struct
             open Parsing
             val byte = byte
             val word16L = word16L
             val word32L = word32L
          end)

      val varint = Varint.varint
         
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
