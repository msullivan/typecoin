
structure Decoder :> DECODER =
   struct

      datatype 'a decoder =
         ANSWER of 'a
       | INPUT of Word8.word -> 'a decoder

      type 'a m = 'a decoder

      fun return x = ANSWER x
      
      fun seq d1 d2 =
         (case d1 of
             ANSWER _ =>
                d2
           | INPUT f =>
                INPUT (fn b => seq (f b) d2))

      fun bind d1 d2 =
         (case d1 of
             ANSWER x =>
                d2 x
           | INPUT f =>
                INPUT (fn b => bind (f b) d2))

      fun wrap f d =
         bind d (fn x => ANSWER (f x))

      exception SyntaxError

      fun count n d =
         let
            fun loop n acc =
               if n = 0 then
                  ANSWER (rev acc)
               else
                  bind d (fn x => loop (n-1) (x :: acc))
         in
            loop n []
         end

      val byte =
         INPUT (fn b => ANSWER b)

      val word16L =
         INPUT (fn b0 =>
         INPUT (fn b1 =>
         ANSWER (Word.orb (Word.<< (ConvertWord.word8ToWord b1, 0w8),
                           ConvertWord.word8ToWord b0))
         ))

      val word32L =
         bind (count 4 byte)
         (fn l => ANSWER (ConvertWord.bytesToWord32L (Bytestring.implode l)))

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

   end
