
structure Go =
   struct

      structure A = Arguments

      fun >> (r, r') = A.seq r r'
      infixr 3 >>

      val flags =
         [A.full "-testnet" (A.exec (fn () => Chain.theChain := Chain.testnet)),
          A.full "-noverify" (A.set Blockchain.neverVerify),
          A.prefix' "-" (A.call (fn flag => (print ("Unknown option "^flag^"\n"); raise A.Usage)))]

      val parser =
         A.scan flags
         >>
         A.eol
         >>
         A.exec Main.main

      val usage = "Usage: server [-testnet] [-noverify]\n"

      val () =
         A.parse parser usage (CommandLine.arguments ())

   end

