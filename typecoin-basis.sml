structure TypeCoinBasis =
struct

  local
      structure BS = Bytestring
      open LF Logic TestUtil
      infixr -->

  (**** "The Basis" ****)
  val byte' = c_app "byte" []
  val hash160' = c_app "hash160" []
  val principal' = c_app "principal" []
  val address' = c_app "address" []

  fun makeArrow 0 = hash160'
    | makeArrow n = byte' --> makeArrow (n-1)

  val bytestring' = c_app "bytestring" []

  val basis_lf = FromNamed.convertSg
      [(T, "byte", EType)] @
      List.tabulate (256, fn i => (O, "b"^Int.fmt StringCvt.HEX i, byte')) @
      [(T, "hash160", EType),
       (O, "hash160_", makeArrow 40),
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
  val principal = c_app' "$" "principal" []
  fun principal_hash k = c_app' "$" "principal_hash" [k]
  val address = c_app' "$" "address" []
  fun address_hash k = c_app' "$" "address_hash" [k]

  val bytestring = c_app' "$" "bytestring" []
  val bs_nil = c_app' "$" "bs_nil" []
  fun bs_cons x xs = c_app' "$" "bs_cons" [x, xs]


  fun byteToLFByte b = c_app' "$" ("b" ^ Word8.fmt StringCvt.HEX b) []

  (* convert a string containing a hash to an LF object of type hash160 *)
  fun hashBytestringToHashObj bs =
      c_app' "$" "hash160_"
      (map byteToLFByte (BS.explode bs))

  fun hashStringToHashObj s = hashBytestringToHashObj (valOf (BS.fromStringHex s))

  fun byteStringToLFByteString bs =
      foldr (fn (x, xs) => bs_cons (byteToLFByte x) xs) bs_nil (BS.explode bs)

  val test_hash_str = "1badd00ddeadbeefcafef00d0123456789abcdef"
  val test_hash = hashStringToHashObj test_hash_str

  val basis = basis

  end

end
