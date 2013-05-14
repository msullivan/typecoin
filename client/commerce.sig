
signature COMMERCE =
   sig

      type btcaddr = Bytestring.string  (* A bitcoin address *)

      datatype output =
         Standard of btcaddr

      exception Invalid
      exception NoKey

      val createTx :
         { inputs : Transaction.coord list,
           outputs : (output * LargeInt.int) list,
           fee : LargeInt.int }
         -> Transaction.tx

   end
