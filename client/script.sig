
signature SCRIPT =
   sig

      datatype inst =
         Const of Bytestring.string
       | Dup
       | Hash160
       | Equalverify
       | Checksig

      val renderInst : inst -> Bytestring.string
      val render : inst list -> Bytestring.string

   end
