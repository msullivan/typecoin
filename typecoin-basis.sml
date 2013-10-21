structure TypeCoinBasis =
struct

  local
      structure BS = Bytestring
      open LF Logic TestUtil
      infixr -->

  (**** "The Basis" ****)
  val byte' = c_app "byte" []
  val hash160' = c_app "hash160" []
  val hash256' = c_app "hash256" []
  val principal' = c_app "principal" []
  val address' = c_app "address" []

  fun makeHashArrow t 0 = t
    | makeHashArrow t n = byte' --> makeHashArrow t (n-1)

  val bytestring' = c_app "bytestring" []

  val basis_lf = FromNamed.convertSg
      [(T, "byte", EType)] @
      List.tabulate (256, fn i => (O, "b"^Int.fmt StringCvt.HEX i, byte')) @
      [(T, "hash160", EType),
       (O, "mk_hash160", makeHashArrow hash160' 20),
       (T, "hash256", EType),
       (O, "mk_hash256", makeHashArrow hash256' 32),
       (T, "principal", EType),
       (O, "principal_hash", hash160' --> principal'),
       (T, "address", EType),
       (O, "address_hash", hash160' --> address'),

       (T, "bytestring", EType),
       (O, "bs_nil", bytestring'),
       (O, "bs_cons", byte' --> bytestring' --> bytestring')

      ]
  val basis = map SConst basis_lf

  in

  val byte = c_app' "$" "byte" []
  val hash160 = c_app' "$" "hash160" []
  val hash256 = c_app' "$" "hash256" []
  val principal = c_app' "$" "principal" []
  fun principal_hash k = c_app' "$" "principal_hash" [k]
  val address = c_app' "$" "address" []
  fun address_hash k = c_app' "$" "address_hash" [k]

  val bytestring = c_app' "$" "bytestring" []
  val bs_nil = c_app' "$" "bs_nil" []
  fun bs_cons x xs = c_app' "$" "bs_cons" [x, xs]


  fun byteToLFByte b = c_app' "$" ("b" ^ Word8.fmt StringCvt.HEX b) []

  (* convert a string containing a hash to an LF object of type hashN,
   * where N is the number of bits. If hashN isn't one of our types, then
   * this will not be very useful. *)
  fun hashBytestringToHashObj bs =
      c_app' "$" ("mk_hash" ^ Int.toString (8 * BS.size bs))
      (map byteToLFByte (BS.explode bs))

  fun hashStringToHashObj s = hashBytestringToHashObj (valOf (BS.fromStringHex s))

  fun bytestringToLFBytestring bs =
      foldr (fn (x, xs) => bs_cons (byteToLFByte x) xs) bs_nil (BS.explode bs)

  val test_hash_str = "1badd00ddeadbeefcafef00d0123456789abcdef"
  val test_hash = hashStringToHashObj test_hash_str

  val basis = basis

  end

end
