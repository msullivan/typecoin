
structure Signature :> SIGNATURE =
   struct

      structure B = Bytestring
      structure BS = Bytesubstring
      structure W = Writer

      fun >>> (w, w') = W.seq w w'
      infixr 3 >>>

      val secp256k1 = EllipticCurveParams.secp256k1



      datatype hash_class = HashAll | HashNone | HashSingle
      type hash_type = hash_class * bool  (* bool indicates anyone-can-pay *)

      fun hashTypeToByte (class, anyoneCanPay) =
         Word8.orb (if anyoneCanPay then 0wx80 else 0w0,
                    (case class of HashAll => 0w1 | HashNone => 0w2 | HashSingle => 0w3))

      fun byteToHashType b =
         let
            val class =
               if Word8.andb (b, 0wx1f) = 0w2 then
                  HashNone
               else if Word8.andb (b, 0wx1f) = 0w3 then
                  HashSingle
               else
                  HashAll

            val anyoneCanPay = Word8.andb (b, 0wx80) <> 0w0

            val () =
               if Word8.andb (b, 0wx7f) < 0w1  orelse  Word8.andb (b, 0wx7f) > 0w3 then
                  Log.long (fn () => "Unusual: hash byte "^ Word8.toString b)
               else
                  ()
         in
            (class, anyoneCanPay)
         end
         

      exception Munge
      exception MungeSingle

      fun munge { version, inputs, outputs, lockTime } i prevScript (hashClass, anyoneCanPay) =
         let
            val () =
               if i < 0 then
                  raise Munge
               else
                  ()

            (* We clear all the input scripts.  This is essential, since you can't sign something
               that has to contain the signature.

               We also put prevScript in place of the ith input script.  This is unnecessary and
               wasteful.  It is unnecessary since prevScript is the output script of the transaction
               that the ith input is spending, and as such is incorporated the transaction's
               hash, which appears in ith input already.  It's wasteful because we need to
               compute a distinct munged transaction (and then hash it) for each of the inputs,
               instead of being able to share one.  Moreover, it necessitates additional
               plumbing so that we can get access to it where munge is called.

               If anyoneCanPay, then we delete all the inputs except for input i (instead of
               merely clearing the scripts).  This means that anyone can modify the transaction
               to add or remove inputs.

               If hashClass is NONE or SINGLE, we clear the inputs' sequence numbers, so anyone
               who supplied those inputs can put in a new version of the transaction.
            *)

            val mungeSequence =
               (case hashClass of
                   HashAll =>
                      (fn sequence => sequence)
                 | HashNone =>
                      (fn sequence => 0w0 : Word32.word)
                 | HashSingle =>
                      (fn sequence => 0w0))

            fun mungeInputs j l =
               (case l of
                   [] =>
                      if i > j then
                         raise Munge
                      else
                         []
                 | {from, script=_, sequence} :: rest =>
                      if i = j then
                         {from=from, script=prevScript, sequence=sequence}
                         :: mungeInputs (j+1) rest
                      else
                         {from=from, script=B.null, sequence=mungeSequence sequence}
                         :: mungeInputs (j+1) rest)
               
            val inputs' =
               if anyoneCanPay then
                  let
                     val {from, script=_, sequence} =
                        List.nth (inputs, i)
                        handle Subscript => raise Munge
                  in
                     [{from=from, script=prevScript, sequence=sequence}]
                  end
               else
                  mungeInputs 0 inputs

            (* see below *)
            fun mungeOutputsSingle j l =
               (case l of
                   [] =>
                      raise MungeSingle
                 | txout :: rest =>
                      if i = j then
                         [txout]
                      else
                         {amount=Word64.~ 0w1, script=B.null}
                         :: mungeOutputsSingle (j+1) rest)

            val outputs' =
               (case hashClass of
                   HashAll =>
                      (* For hash class ALL, we leave the outputs alone.  All the outputs are signed.
                         This is the normal usage.
                      *)
                      outputs

                 | HashNone =>
                      (* For hash class NONE, we delete all the outputs.  This means that anyone
                         can change the transaction's outputs.  (Why would you ever want this?)
                      *)
                      []

                 | HashSingle =>
                      (* For hash class SINGLE, we (in effect) delete all the outputs except the
                         ith output.  Thus all the outputs can be changed except for output i.
                         Note that i, which is an index into the inputs, in this case does double-duty
                         and serves as an index into the outputs as well.

                         The way this is accomplished is a bit strange.  The outputs after i are
                         simply deleted.  However, the outputs before i are set to "Null", where
                         "Null" is an empty script and amount = -1.
                      *)
                      mungeOutputsSingle 0 outputs)
         in
            { version=version, inputs=inputs', outputs=outputs', lockTime=lockTime }
         end


      
      (* hashTx tx i script hashByte

         Munges and hashes the transaction.

         tx, i, and script are as for munge.
         0 <= i < the number of tx's inputs

         (We have to take the hash type as a byte, rather than symbolically, because it
         has to be appended to the written signature, even in a nonstandard representation.)
      *)
      fun hashTx tx i script hashByte =
         let
            val tx' = munge tx i script (byteToHashType hashByte)

            val str =
               W.write
               (Transaction.writer tx'
                >>>
                W.word32L (ConvertWord.word8ToWord32 hashByte))
         in
            SHA256.hashBytes (SHA256.hashBytes str)
         end
         (* Here we replicate a bug from the reference client.  Signature checking
            ought to fail when transaction munging fails, but it doesn't.  In the
            reference client, the tranaction munger returns 1 to indicate the error
            condition, but the munger is part of the transaction hashing function
            (SignatureHash), so that 1 is just interpreted as the hash.

            Of the three conditions that can cause munging to fail, the first two
            should never happen: if i is outside the range of inputs, we never would
            have obtained a script in the first place.  However, the third condition
            (i outside the range of *outputs* when the hash class is SINGLE) can
            actually happen.

            The first two failure conditions raise Munge, while the third raises
            MungeSingle.  Accordingly, we fail for a violated preondition if Munge
            is raised, but we generate a "hash value" of 1 if MungeSingle is raised.
         *)
         handle
            Munge => raise (Fail "precondition")
          | MungeSingle =>
               (
               Log.long (fn () => "Unusual: HASH_SINGLE bug");
               ConvertIntInf.toFixedBytesL (32, 1)
               )



      exception CheckFailed
      fun checkOne tx i script sg pubkey =
         let
            val sgsz = B.size sg

            val () =
               if sgsz < 1 then
                  raise CheckFailed
               else
                  ()

            val hashByte = B.sub (sg, sgsz-1)

            val hash = hashTx tx i script hashByte

            val pubkey' =
               ECDERp.decodePubkey (secp256k1, pubkey)
               handle ECDERp.Invalid => raise CheckFailed

            val sg' =
               ECDERp.decodeSg (B.substring (sg, 0, sgsz-1))
               handle ECDERp.Invalid => raise CheckFailed
         in
            ECDSAp.verify (secp256k1, pubkey', hash, sg')
         end
         handle CheckFailed => false


      (* extra = length pubkeys - length sigs *)
      fun checkMany tx i script sigs pubkeys extra =
         if extra < 0 then
            (* Can't succeed now, so give up. *)
            false
         else
            (case sigs of
                [] =>
                   true
              | sg :: sigs' =>
                   (case pubkeys of
                       [] =>
                          raise (Fail "impossible")
                     | pubkey :: pubkeys' =>
                          if checkOne tx i script sg pubkey then
                             checkMany tx i script sigs' pubkeys' extra
                          else
                             checkMany tx i script sigs pubkeys' (extra-1)))



      (* Determine whether str1 and str2 are equal up to index sz. *)
      fun compareTo (str1 : BS.substring) (str2 : B.string) sz =
         let
            fun loop i =
               if i >= sz then
                  true
               else if BS.sub (str1, i) = B.sub (str2, i) then
                  loop (i+1)
               else
                  false
         in
            loop 0
         end

      fun sliceBetween str strSuffix =
         BS.slice (str, 0, SOME (BS.size str - BS.size strSuffix))


      (* searchAndDestroy script target

         Scan through script and delete instances of target, which must be the encoding of an instruction.
      *)
      fun searchAndDestroy script target =
         let
            val targetsz = B.size target

            (* We don't want to accrete the script one instruction at a time, so the first element of acc
               is implicitly the segment between lastscript and curscript.
            *)
            fun loop lastscript curscript acc found =
               if BS.size curscript < targetsz then
                  if found then
                     (
                     Log.long (fn () => "Unusual: search and destroy");
                     BS.full (BS.concat (rev (sliceBetween lastscript curscript :: acc)))
                     )
                  else
                     (* Optimize this case, which is almost always what happens. *)
                     script
               else if compareTo curscript target targetsz then
                  let
                     val newscript = BS.slice (curscript, targetsz, NONE)
                  in
                     loop newscript newscript (sliceBetween lastscript curscript :: acc) true
                  end
               else
                  let
                     val (_, newscript) = Reader.readS Script.instReader curscript
                  in
                     loop lastscript newscript acc found
                  end
         in
            loop script script [] false
         end



      val strCodeseparator = W.write (Script.instWriter Script.Codeseparator)

      fun verify tx i script sigs pubkeys =
         (* Basically we just invoke checkMany here.  But first we have to launder the script
            in two ways: (1) we remove any instances of Codeseparator, and (2) we remove
            any instances of Const(str), where str is one of the signatures.

            There is no reason for any of this.  As noted above, including the script in the
            munged transaction at all is unnecessary and wasteful, so no purpose can be served
            by laundering the script first.  Moreover, even if that were not so, it's hard to
            see any purpose that would be served by the laundering.

            (Indeed, #2 seems to suggest some confusion about how verification works.  The comment
            in the reference implementation reads "Drop the signatures, since there's no way for a
            signature to sign itself", but we are laundering the *output* script, which existed
            prior to any transaction attempting to spend it.)

            For #1 it's at least harmless to do this.  For #2 it is technically not harmless.
            If one of the signatures just happens to be a string that appears in script, the script
            will be modified to remove it.  But the signer expected it to be there, so verification
            will spuriously fail.  The signer can't anticipate this either, since you can't
            know the signature unitl after you sign it.  (Thus, they are creating exactly the
            problem it seems they were trying to avoid.)

            Fortunately, the odds against a random collision between two (roughly) 512-bit numbers
            are astronomical, so this problem can neglected.  Also, output scripts rarely contain
            constants that size anyway.
         *)
         let
            val script' = searchAndDestroy script strCodeseparator
            val script' =
               List.foldl
               (fn (sg, curscript) =>
                   let
                      val target = W.write (Script.instWriter (Script.Const sg))
                   in
                      searchAndDestroy curscript target
                   end)
               script'
               sigs
         in
            checkMany tx i (BS.string script') sigs pubkeys (length pubkeys - length sigs)
         end



      fun sign tx i script hashType privkey =
         let
            val hashByte = hashTypeToByte hashType

            (* Don't bother to launder the script.  (See above.)  We assume it does not contain
               Codeseparator, and nothing can be done about signatures colliding with the script.
               Fortunately, such collisions are extremely unlikely.
            *)

            val hash = hashTx tx i script hashByte
            
            val sg = ECDSAp.sign (secp256k1, privkey, hash)
         in
            B.^ (ECDERp.encodeSg sg, B.str hashByte)
         end

   end
