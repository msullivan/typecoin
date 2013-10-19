
structure Commerce :> COMMERCE =
   struct

      structure B = Bytestring
      structure BS = Bytesubstring
      structure S = Script
      structure W = Writer

      fun >>> (w, w') = W.seq w w'
      infixr 3 >>>
      

      structure BytestringHashable =
         struct
            type t = B.string
            val eq = B.eq

            fun hash str =
               (* The front bytes are pretty much random, so just use those. *)
               ConvertWord.wordLgToWord (PackWord32Big.subVec (str, 0))
         end
      structure T = HashTable (structure Key = BytestringHashable)



      val bfh = valOf o Bytestring.fromStringHex

      (* XX Need to implement a good way to find transactions by hash.  For now, just hardcode a table. *)
      val txTable : (int * int) T.table = T.table 20
      val () =
         app (fn (hash, blocknum, i) => T.insert txTable (B.rev hash) (blocknum, i))
         [
         (bfh "8f822a04c5f9f30a129acfef44749565666390bd3f4d571db428fd318ce6eeef", 228648, 115)
         ]
      fun findInput hash = T.find txTable hash


      (* XX Need to implement a good way to find keys.  For now, just hardcode a table. *)
      val keyTable : (ECDSAp.privkey * bool) T.table = T.table 20
      val () =
         app (fn (addr, privkey, compressed) => T.insert keyTable addr (privkey, compressed))
         [
         (bfh "3a6dbfaacb91dad6ae8c645c8947d0efe5034b19",
          74546373234046248893384993299922516061981751989874750013347752163560598582354,
          true)
         ]

      fun findKey addr = T.find keyTable addr



      val dhash = SHA256.hashBytes o SHA256.hashBytes
      val secp256k1 = EllipticCurveParams.secp256k1

      val maxAmount = Word64.toLargeInt (Word64.~ 0w1)



      type btcaddr = Bytestring.string  (* A bitcoin address *)

      datatype output =
         Standard of btcaddr

      exception Invalid
      exception NoKey


      fun resolveInput (txhash, n) =
         (case findInput txhash of
             NONE =>
                (* Input not found *)
                raise Invalid
           | SOME (blocknum, i) =>
                let
                   val blstr = Blockchain.dataByNumber blocknum
                   val (_, txs) = Block.readBlock blstr
                   val tx = List.nth (txs, i)

                   val () =
                      (* This shouldn't be necessary, but let's double-check that we've got
                         the right transaction.
                      *)
                      if B.eq (txhash, dhash (Transaction.writeTx tx)) then
                         ()
                      else
                         raise (Fail "wrong transaction")

                   val {amount, script} =
                      List.nth (#outputs tx, n)
                      handle Subscript =>
                         (* Input transaction doesn't have this many outputs *)
                         raise Invalid
                in
                   (txhash, n, Word64.toLargeInt amount, script)
                end)
         

      fun synthesize output =
         (case output of
             Standard addr =>
                [S.Dup, S.Hash160, S.Const addr, S.EqualVerify, S.Checksig])


      fun analyze l =
         (case l of
             [S.Dup, S.Hash160, S.Const addr, S.EqualVerify, S.Checksig] =>
                Standard addr
           | _ =>
                (* Don't understand this script. *)
                raise Invalid)


      fun createTx { inputs, outputs, fee } =
         let
            val () =
               (* Check that the amounts are in range. *)
               app (fn (_, amount) => 
                          if amount <= 0 orelse amount > maxAmount then
                             raise Invalid
                          else
                             ()) outputs

            val inputs' = map resolveInput inputs

            val () =
               (* Check that input amount = output amount. *)
               if
                  foldl (fn ((_, _, amount, _), total) => amount + total) 0 inputs'
                  =
                  foldl (fn ((_, amount), total) => amount + total) fee outputs
               then
                  ()
               else
                  raise Invalid

            val pretxins =
               map
               (fn (hash, n, _, _) =>
                   { from=(hash, n), script=B.null, sequence=0wxffffffff })
               inputs'

            val txouts =
               map
               (fn (output, amount) => { amount=Word64.fromLargeInt amount, script=Script.writeScript (synthesize output) })
               outputs

            val pretx : Transaction.tx =
               Transaction.mkTx { inputs=pretxins, outputs=txouts, lockTime=0w0 }

            fun signloop j acc l =
               (case l of
                   [] => rev acc
                 | (txinhash, n, _, ioscript) :: rest =>
                      let
                         val script =
                            (* Figure out what ioscript is looking for. *)
                            (case analyze (Script.readScript ioscript) of
                                Standard addr =>
                                   (case findKey addr of
                                       NONE =>
                                          (* Don't have the key. *)
                                          raise NoKey
                                     | SOME (privkey, compress) =>
                                          let
                                             val sg = Signature.sign pretx j ioscript (Signature.HashAll, false) privkey

                                             val pubkey = ECDSAp.privkeyToPubkey (secp256k1, privkey)
                                             val pubkeystr =
                                                if compress then
                                                   ECDERp.encodePubkeyCompressed (secp256k1, pubkey)
                                                else
                                                   ECDERp.encodePubkey (secp256k1, pubkey)
                                          in
                                             S.writeScript
                                             [S.Const sg, S.Const pubkeystr]
                                          end))

                         val txin = { from=(txinhash, n), script=script, sequence=0wxffffffff }
                      in
                         signloop (j+1) (txin :: acc) rest
                      end)

            val txins = signloop 0 [] inputs'
         in
            Transaction.mkTx { inputs=txins, outputs=txouts, lockTime=0w0 }
         end

   end
