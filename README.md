This is a demo implementation of [Typecoin]
 (https://www.msully.net/stuff/typecoin.pdf).

Typecoin generalizes Bitcoin to, instead of having transactions deal
merely in monetary amounts, have transactions instead deal in
propositions in an expressive linear logic.
This has applications in consumable credentials for proof-carrying
authorization systems and in smart contracts.

We have an implementation of the Typecoin logic and typechecker that
can interact with Bitcoin via a new Bitcoin client written in SML.

There is more work that would be needed for this to be useful in
practice, including but probably not limited to:
 * A better way of constructing Typecoin transactions than
   by writing syntax trees by hand.
 * Crypto code that wasn't written in SML by non-experts
 * A system for maintaining a "wallet" of Typecoin propositions


Building and using
-----------
 * The Bitcoin client uses a client/server architecture in which a
   server runs continuously, listening on the Bitcoin network and
   verifying transactions and a client library exists to allow other
   programs to query the server for information or ask it to transmit
   a new transaction.
 * We use submodules to fetch the cmlib and stilts dependencies,
   so you must run:
    `git submodule init && git submodule update`
 * The server program should be built with mlton:
    `mlton client/server.mlb`
 * The Bitcoin client library can be built with either MLton or
   SML/NJ, though since we don't ship any useful client programs that
   use the library, the best way to poke around with it is by using
   SML/NJ's interactive REPL.
    - In the SML/NJ REPL, `CM.make "client/sources.cm";`
    - MLton client build file is `client/client.mlb`
 * The Typecoin typechecker must be built with SML/NJ:
   `CM.make "typecoin/typecoin.cm";`
 * See typecoin/batch/README.md for notes on the batch server
   (which is exceptionally demo-quality).


Dependencies
-----------
 * [cmlib] (https://github.com/standardml/cmlib/), for lots of various useful stuff
 * [stilts] (https://github.com/j4cbo/stilts/), for its sqlite
   bindings (which are used in the demo batch server)
 * [IOtype] (http://yumegakanau.org/code/iotype/), for auto-generating
   code to serialize and deserialize Typecoin transactions. This is not
   a build dependency, since we (terribly) just commit the generated
   file to source control, but it is needed to make changes to the
   data structures.
