
signature VERIFY =
   sig

      val decodeDifficulty : Word32.word -> IntInf.int

      (* We trust that the longest block chain contains only correct blocks.  This does
         the small set of checks necessary to ensure that this block (a) doesn't fool us
         into thinking it's on a longer chain than it really is, and (b) hasn't been
         altered.  That is, it checks:

         1. The claimed hash satisfies the claimed difficulty.
         2. The merkle root is correct.

         #1 ensures that the block doesn't pretend to be harder than it is.  #2
         ensure that the block's contents comport with its header.
      *)
      val verifyBlockGross : EBlock.eblock -> bool

      (* Assumes that the block has already passed verifyBlockGross. *)
      val verifyBlock : EBlock.eblock -> bool

      val verifyTx : Transaction.tx -> bool

   end
