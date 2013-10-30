
signature RPC_MESSAGE =
   sig

      datatype request =
         CloseChannel
       | ShutdownServer
       | BlockMember of Bytestring.string
       | LookupBlock of Bytestring.string
       | BlockPrimary of Bytestring.string
       | LastBlock
       | TotalDifficulty
       | BlockByNumber of int
       | LookupTx of Bytestring.string
       | TxByNumber of int * int
       | Inject of Transaction.tx

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
