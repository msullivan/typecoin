
signature SIGNATURE =
   sig

      datatype hash_class = HashAll | HashNone | HashSingle
      type hash_type = hash_class * bool  (* bool indicates "anyone can pay" *)

      val hashTypeToByte : hash_type -> Word8.word
      val byteToHashType : Word8.word -> hash_type

      (* munge tx i script hashType

         Modifies a transaction to prepare it for signing:

         - tx is the transaction to munge
         - i is the index of the input to be verified
           (and for hashType=(SINGLE, _), also the output not to be cleared)
         - script is the script invoking the signature check
         - hashType indicates the hashing rules

         Raises Munge if i < 0 or i >= number of tx's inputs
         Raises MungeSingle if hashType=(SINGLE, _) and i >= number of tx's outputs
      *)
      exception Munge
      exception MungeSingle
      val munge : Transaction.tx -> int -> Bytestring.string -> hash_type -> Transaction.tx


      (* check tx i script sigs pubkeys

         Checks that each public key is matched by a digital signature.  Each sig matches at most
         one public key, and the matches must appear in the same order in both lists.

         tx, i, and script are as for munge, except script is taken as a bytesubstring for convenience.
         0 <= i < the number of tx's inputs
      *)
      val verify : Transaction.tx -> int -> Bytesubstring.substring -> Bytestring.string list -> Bytestring.string list -> bool


      (* sign tx i script hashType privkey

         Digitally signs the transaction.

         tx, i, script, and hashType are as for munge.
         script must not contain Codeseparator.
      *)
      val sign : Transaction.tx -> int -> Bytestring.string -> hash_type -> ECDSAp.privkey -> Bytestring.string

   end
