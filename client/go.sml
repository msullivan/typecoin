
structure Go =
   struct

      structure A = Arguments

      fun >> (r, r') = A.seq r r'
      infixr 3 >>

      val flags =
         [
         A.full "-ignoregetblocks" (A.clear Constants.answerGetblocks),
         A.full "-noverify" (A.set Blockchain.neverVerify),
         A.full "-rpcport" (A.assign Constants.rpcPort A.int),
         A.full "-testnet" (A.exec (fn () => Chain.theChain := Chain.testnet)),
         A.prefix' "-" (A.call (fn flag => (print ("Unknown option "^flag^"\n"); raise A.Usage)))
         ]

      val parser =
         A.scan flags
         >>
         A.eol
         >>
         A.exec Main.main

      val usage = "Usage: server [options ...]\n    -ignoregetblocks    don't respond to getblocks requests\n    -noverify           don't verify any blocks\n    -rpcport <number>   port for rpc server\n    -testnet            run on testnet\n"

      val () =
         A.parse parser usage (CommandLine.arguments ())

   end

