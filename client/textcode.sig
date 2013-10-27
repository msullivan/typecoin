
(* Bitcoin "addresses".  That is, base 58 encodings of byte strings. *)

signature TEXTCODE =
   sig

      exception Invalid

      val decodeAddress : string -> Bytestring.string
      val encodeAddress : Bytestring.string -> string

      val decodeAddressTestnet : string -> Bytestring.string
      val encodeAddressTestnet : Bytestring.string -> string

      (* bool indicates compressed public key *)
      val decodePrivkey : string -> ECDSAp.privkey * bool
      val encodePrivkey : ECDSAp.privkey * bool -> string

      (* bool indicates compressed public key *)
      val decodePrivkeyTestnet : string -> ECDSAp.privkey * bool
      val encodePrivkeyTestnet : ECDSAp.privkey * bool -> string

   end
