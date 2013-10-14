structure TypeCoinBasis =
struct

  local
      open LF Logic TestUtil
      infixr -->

  (**** "The Basis" ****)
  val nibble' = c_app "nibble" []
  val hash160' = c_app "hash160" []
  val principal' = c_app "principal" []
  fun principal_hash k = c_app "principal_hash" [k]
  val address' = c_app "address" []
  fun makeArrow 0 = hash160'
    | makeArrow n = nibble' --> makeArrow (n-1)
  val basis_lf = FromNamed.convertSg
      [(T, "nibble", EType)] @
      List.tabulate (16, fn i => (O, "n"^Int.fmt StringCvt.HEX i, nibble')) @
      [(T, "hash160", EType),
       (O, "hash160_", makeArrow 40),
       (T, "principal", EType),
       (O, "principal_hash", hash160' --> principal'),
       (T, "address", EType),
       (O, "address_hash", hash160' --> address')
      ]
  val basis = map SConst basis_lf

  in

  val nibble = c_app' "$" "nibble" []
  val hash160 = c_app' "$" "hash160" []
  val principal = c_app' "$" "principal" []
  fun principal_hash k = c_app' "$" "principal_hash" [k]
  val address = c_app' "$" "address" []
  fun address_hash k = c_app' "$" "address_hash" [k]

  (* convert a string containing a hash to an LF object of type hash160 *)
  fun hashStringToHashObj s =
      c_app' "$" "hash160_"
      (map (fn c => c_app ("n" ^ str (Char.toUpper c)) []) (explode s))
  val test_hash = "1badd00ddeadbeefcafef00d0123456789abcdef"

  val basis = basis

  end

end
