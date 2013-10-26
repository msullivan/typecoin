
signature COMMERCE =
   sig

      type btcaddr = Bytestring.string  (* A bitcoin address *)

      datatype output =
         Standard of btcaddr

      exception Invalid
      exception NoKey
      
      val synthesize : output -> Script.inst list
      val analyze : Script.inst list -> output

      val createTx :
         { inputs : Transaction.coord list,
           outputs : (output * LargeInt.int) list,
           fee : LargeInt.int,
           keys : EllipticCurveCryptoFp.privkey list }
         -> Transaction.tx

   end
