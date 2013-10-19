
signature INTERPRET =
   sig

      type value = Bytestring.string
      type stack = value list

      exception Reject

      (* exec tx i script stack

         Executes script with stack, returning the resulting stack.  Raises Reject if execution fails.

         tx is the transaction being validated, and i is the index of the input of that transaction
         being validated.  These are used in signature checking.

         Unfortunately, we have to take the script in binary form, not symbolic form, because the
         signature checking operations depend on the actual binary representation of the script.
         (For no good reason, I might add.)
      *)
      val exec : Transaction.tx -> int -> Bytestring.string -> stack -> stack

      val passes : stack -> bool

   end
