
signature VERIFY =
   sig

      val decodeDifficulty : Word32.word -> IntInf.int

      (* We trust that the longest block chain contains only correct blocks.  This does
         the small set of checks necessary to ensure that this block (a) doesn't fool us
         into thinking it's on a longer chain than it really is, and (b) hasn't been
         altered.  That is, it checks:

         1. The block parses.
         2. The claimed hash satisfies the claimed difficulty.
         3. The merkle root is correct.

         #1 is a prequisite to any checking.  #2 ensures that the block doesn't pretend to
         be harder than it is.  #3 ensures that the block's contents comport with its header.
      *)
      val verifyBlockGross : EBlock.eblock -> bool


      (* verifyTx getTx tx
         
            getTx coord
            -----------
            If coord is valid and unspent, marks it as spent (if appropriate), and returns SOME of transaction.
            Otherwise, returns NONE.

         Returns true iff tx passes verification.  Calls getTx on all tx's inputs.
      *)
      val verifyTx : (Transaction.coord -> Transaction.tx option) -> Transaction.tx -> bool



      (* verifyStoredBlock pos eblock

         If    eblock has passed verifyBlockGross
               eblock is stored at position pos
         then  returns true iff eblock passes verification
      *)
(* XX *)
      val verifyBlock : EBlock.eblock -> bool

   end
