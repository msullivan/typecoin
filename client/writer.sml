
structure Writer :> WRITER =
   struct

      structure B = Bytestring

      exception InvalidData

      type writer = B.string list -> B.string list

      fun null acc = acc

      fun seq wr1 wr2 acc = wr2 (wr1 acc)

      fun seql l acc =
         List.foldl (fn (wr, acc) => wr acc) acc l

      fun repeat n wr acc =
         let
            fun loop i acc =
               if i = 0 then
                  acc
               else
                  loop (i-1) (wr acc)
         in
            loop n acc
         end

      fun list f l acc =
         List.foldl (fn (x, acc') => f x acc') acc l
         

      fun byte w acc =
         B.str w :: acc

      fun word16B w acc =
         if w > 0wxffff then
            raise InvalidData
         else
            B.substring (ConvertWord.wordToBytesB w, 2, 2) :: acc

      fun word16L w acc =
         if w > 0wxffff then
            raise InvalidData
         else
            B.substring (ConvertWord.wordToBytesL w, 0, 2) :: acc

      fun word32L w acc =
         ConvertWord.word32ToBytesL w :: acc

      fun word64L w acc =
         ConvertWord.word64ToBytesL w :: acc

      fun bytes s acc = s :: acc

      fun bytesS s acc = Bytesubstring.string s :: acc



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
         seq (varint (length l)) (list f l)


      fun write f =
         B.concat (rev (f []))
      
   end
