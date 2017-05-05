This is essentially a *sketch* of how a typecoin batch server would be
built. It is not, as it stands, a workable typecoin batch server.

Critically, it lacks authentication, which is a pretty bad problem!

--
 * Both the batch server and the batch server client library are built
   with:
     `CM.make "typecoin/batch/batch.cm";`
 * The server is launched with: `BatchServer.main ();`
 * The client operates through the BatchClient structure.
 * To build the demo batch server, you will need some additional packages
   that stilts requires. On debian-like systems, these are:
     libckit-smlnj libmlnlffi-smlnj ml-nlffigen ml-lex ml-yacc ml-lpt
