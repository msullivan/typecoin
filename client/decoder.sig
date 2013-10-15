
(* A more abstract reading monad that doesn't depend on streams, but
   doesn't offer branching.  Probably also less efficient that Reader.
*)

(* Todo: Maybe factor out the commonality between this and READER? *)

signature DECODER =
   sig

      datatype 'a decoder =
         ANSWER of 'a
       | INPUT of Word8.word -> 'a decoder

      include MONAD where type 'a m = 'a decoder

      val wrap : ('a -> 'b) -> 'a decoder -> 'b decoder

      exception SyntaxError

      val byte : Word8.word decoder
      val word16L : Word.word decoder
      val word32L : Word32.word decoder
      val varint : int decoder

   end