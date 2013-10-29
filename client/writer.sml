
structure Writer :> WRITER =
   struct

      structure B = Bytestring
      structure O = Output

      exception InvalidData

      type writer = B.string O.output

      structure M = MonadUtilFun (structure Monad = OutputMonad (type elem = B.string))
      open M

      val null = O.nothing

      fun repeat n wr =
         let
            fun loop i acc =
               if i = 0 then
                  acc
               else
                  loop (i-1) (seq acc wr)
         in
            loop n null
         end

      val list = MonadList.appM


      fun byte w = O.output (B.str w)

      fun word16B w =
         if w > 0wxffff then
            raise InvalidData
         else
            O.output (B.substring (ConvertWord.wordToBytesB w, 2, 2))

      fun word16L w =
         if w > 0wxffff then
            raise InvalidData
         else
            O.output (B.substring (ConvertWord.wordToBytesL w, 0, 2))

      fun word32B w =
         O.output (ConvertWord.word32ToBytesB w)

      fun word32L w =
         O.output (ConvertWord.word32ToBytesL w)

      fun word64L w =
         O.output (ConvertWord.word64ToBytesL w)

      val bytes = O.output

      fun bytesS s = O.output (Bytesubstring.string s)



      fun varint n =
         if n <= 252 then
            byte (Word8.fromInt n)
         else if n <= 0xffff then
            seq (byte 0wxfd) (word16L (Word.fromInt n))
         else if n <= 0x3fffffff then
            seq (byte 0wxfe) (word32L (Word32.fromInt n))
         else
            (* We allow sizes only up to 2^30. *)
            raise InvalidData
            
         
      fun bytesVar s = seq (varint (B.size s)) (bytes s)

      fun bytesPad n s =
         let
            val sz = B.size s
         in
            if sz > n then
               raise InvalidData
            else
               seq (bytes s) (bytes (Word8Array.vector (Word8Array.array (n-sz, 0w0))))
         end

      fun bytesFixed n s =
         if B.size s <> n then
            raise InvalidData
         else
            bytes s

      fun varlist f l =
         Output.lazy (fn () => bind (varint (length l)) (fn () => list f l))


      fun write wr =
         B.concat (O.append wr)

      fun writeOutstream outs wr =
         O.app (fn str => BinIO.output (outs, str)) wr

   end
