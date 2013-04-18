
structure Writer :> WRITER =
   struct

      structure B = Bytestring

      exception InvalidData

      fun sizemark n =
         if n < 253 then
            B.str (Word8.fromInt n)
         else
            raise (Fail "large field unimplemented")

      type writer = B.string list -> B.string list

      fun seq (wr1, wr2) acc = wr2 (wr1 acc)

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
         

      fun byte w acc =
         B.str w :: acc

      fun word16B w acc =
         if w > 0wxffff then
            raise InvalidData
         else
            B.substring (ConvertWord.wordToBytesB w, 2, 2) :: acc

      fun word32L w acc =
         ConvertWord.word32ToBytesL w :: acc

      fun word64L w acc =
         ConvertWord.word64ToBytesL w :: acc

      fun bytes s acc = s :: acc
         
         
      fun bytesVar s = seq (bytes (sizemark (B.size s)), bytes s)

      fun bytesPad n s =
         let
            val sz = B.size s
         in
            if sz > n then
               raise InvalidData
            else
               seq (bytes s, bytes (Word8Array.vector (Word8Array.array (n-sz, 0w0))))
         end            

      fun write f =
         B.concat (rev (f []))
      
   end
