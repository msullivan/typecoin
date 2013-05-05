
structure Base58 :> BASE58 =
   struct

      structure B = Bytestring

      val b58digits = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
      
      val b58map =
         let
            val arr = Array.array (128, ~1)
      
            fun loop n =
               if n >= 58 then
                  ()
               else
                  (
                  Array.update (arr, Char.ord (String.sub (b58digits, n)), n);
                  loop (n+1)
                  )
      
            val () = loop 0
         in
            Array.vector arr
         end


      fun to f radix acc (n:IntInf.int) =
         if n = 0 then
            acc
         else
            to f radix (f (n mod radix) :: acc) (n div radix)


      fun from f radix (acc:IntInf.int) i limit =
         if i >= limit then
            acc
         else
            from f radix (acc * radix + f i) (i+1) limit
         

      fun replicate i x acc =
         if i <= 0 then
            acc
         else
            replicate (i-1) x (x :: acc) 

            
      fun checksum s =
         (* This is an awfully silly way to calculate a checksum. *)
         B.substring (SHA256.hashBytes (SHA256.hashBytes s), 0, 4)



      fun leadingZeros i s =
         if B.sub (s, i) <> 0w0 then
            i
         else
            leadingZeros (i+1) s

      fun encode s =
         let
            val sum = checksum s

            val s' = B.^ (s, sum)
            val zs = leadingZeros 0 s'

            val num = from (fn i => IntInf.fromInt (Word8.toInt (B.sub (s', i)))) 256 0 0 (B.size s')
            val chars = to (fn n => String.sub (b58digits, IntInf.toInt n)) 58 [] num
         in
            String.implode (replicate zs #"1" chars)
         end



      fun leadingZeros58 i str =
         if String.sub (str, i) <> #"1" then
            i
         else
            leadingZeros58 (i+1) str

      exception Invalid

      fun decode str =
         let
            val zs = leadingZeros58 0 str

            val num =
               from
                  (fn i => 
                      let
                         val x = Vector.sub (b58map, Char.ord (String.sub (str, i)))
                      in
                         if x = ~1 then
                            raise Invalid
                         else
                            IntInf.fromInt x
                      end)
                  58 0 0 (String.size str)

            val bytes = to (fn n => Word8.fromInt (IntInf.toInt n)) 256 [] num
            val s = B.implode (replicate zs 0w0 bytes)

            val () =
               if B.size s < 4 then
                  raise Invalid
               else
                  ()

            val s' = B.substring (s, 0, B.size s - 4)
            val sum = B.substring (s, B.size s - 4, 4)
            val sum' = checksum s'
         in
            if B.eq (sum, sum') then
               s'
            else
               raise Invalid
         end

   end
