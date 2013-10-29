
signature RPC_MESSAGE =
   sig

      datatype request =
         CloseChannel
       | ShutdownServer
       | Inject of Transaction.tx
       | LookupTx of Bytestring.string
       | LastBlock
       | PositionByNumber of int

      datatype response =
         False
       | True
       | Int of int
       | Integer of IntInf.int  (* unsigned for now *)
       | String of Bytestring.string
       | Cons of response * response
       | Exception of response
       | Syntax
       | Transaction of Transaction.tx

      val requestWriter : request -> Writer.writer
      val requestReader : request Reader.reader

      val responseWriter : response -> Writer.writer
      val responseReader : response Reader.reader

   end
