structure TypeCoinBasis =
struct

  local
      structure BS = Bytestring
      open LF Logic TestUtil Const
      infixr --> infix <--


  (**** "The Basis" ****)
  val byte' = c_app "byte" []
  val hash160' = c_app "hash160" []
  val hash256' = c_app "hash256" []
  val principal' = c_app "principal" []
  val address' = c_app "address" []

  fun makeHashArrow t 0 = t
    | makeHashArrow t n = byte' --> makeHashArrow t (n-1)

  val bytestring' = c_app "bytestring" []

  val bit' = c_app "bit" []
  val b0' = c_app "b0" []
  val b1' = c_app "b1" []
  val number' = c_app "number" []
  val pos' = c_app "pos" []
  val leading_one' = c_app "leading-one" []
  fun bsnoc' bs b = c_app "::" [bs, b]
  val zero' = c_app "zero" []
  fun pos_num' n = c_app "pos-num" [n]

  fun plus' n1 n2 n3 = c_app "plus" [n1, n2, n3]
  fun plusp' n1 n2 n3 = c_app "plusp" [n1, n2, n3]
  fun inc' n1 n2 = c_app "inc" [n1, n2]

  val coord' = c_app "coord" []

  fun byteFmt b = StringCvt.padLeft #"0" 2 (Word8.fmt StringCvt.HEX b)

  fun v s = HVar (~1, s)
  fun var s = EApp (v s, SNil)
  val [N, M, P, Q] =
      map var ["N", "M", "P", "Q"]


  val basis_lf = FromNamed.convertSg (
      [(T, "byte", EType)] @
      List.tabulate (256, fn i => (O, "b"^ byteFmt (Word8.fromInt i), byte')) @
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
       (O, "bs_cons", byte' --> bytestring' --> bytestring'),

       (* Need to have regular binary *)
       (* Represented in this sort of funny way with an leading one
        * so we have adequecy. *)
       (T, "bit", EType),
       (O, "b0", bit'),
       (O, "b1", bit'),

       (T, "pos", EType),
       (O, "leading-one", pos'),
       (O, "::", pos' --> bit' --> pos'),

       (T, "number", EType),
       (O, "zero", number'),
       (O, "pos-num", pos' --> number'),

       (T, "coord", EType),
       (O, "mk_coord", hash256' --> number' --> coord'),

       (* Binary arithmetic! *)
       (T, "inc", pos' --> pos' --> EType),
       (O, "inc/end", inc' leading_one' (bsnoc' leading_one' b0')),
       (O, "inc/0",
        EPi ("N", pos',
             inc' (bsnoc' N b0') (bsnoc' N b1'))),
       (O, "inc/1",
        EPi ("N", pos', EPi ("M", pos',
             inc' (bsnoc' N b1') (bsnoc' M b0') <-- inc' N M))),

       (* Do we care that the proofs be unique? *)
       (T, "plusp", pos' --> pos' --> pos' --> EType),
       (O, "plusp/1/n",
        EPi ("N", pos', EPi ("M", pos',
             plusp' leading_one' N M <-- inc' N M))),
       (O, "plusp/n/1",
        EPi ("N", pos', EPi ("M", pos',
             plusp' N leading_one' M <-- inc' N M))),
       (O, "plusp/0/0",
        EPi ("N", pos', EPi ("M", pos', EPi ("P", pos',
             plusp' (bsnoc' N b0') (bsnoc' M b0') (bsnoc' P b0')
             <-- plusp' N M P)))),
       (O, "plusp/1/0",
        EPi ("N", pos', EPi ("M", pos', EPi ("P", pos',
             plusp' (bsnoc' N b1') (bsnoc' M b0') (bsnoc' P b1')
             <-- plusp' N M P)))),
       (O, "plusp/0/1",
        EPi ("N", pos', EPi ("M", pos', EPi ("P", pos',
             plusp' (bsnoc' N b0') (bsnoc' M b1') (bsnoc' P b1')
             <-- plusp' N M P)))),
       (O, "plusp/1/1",
        EPi ("N", pos', EPi ("M", pos', EPi ("P", pos', EPi ("Q", pos',
             plusp' (bsnoc' N b1') (bsnoc' M b1') (bsnoc' Q b0')
             <-- plusp' N M P
             <-- inc' P Q))))),


       (T, "plus", number' --> number' --> number' --> EType),
       (O, "plus/0/n",
        EPi ("N", number', plus' zero' N N)),
       (O, "plus/n/0",
        EPi ("N", number', plus' N zero' N)),
       (O, "plus/n/n",
        EPi ("N", pos', EPi ("M", pos', EPi ("P", pos',
             plus' (pos_num' N) (pos_num' M) (pos_num' P)
             <-- plusp' N M P)))),


       (T, "void", EType)
      ])
  val basis = map SConst basis_lf

  in

  (* Some crap *)
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

  val bit = c_app' "$" "bit" []
  val b0 = c_app' "$" "b0" []
  val b1 = c_app' "$" "b1" []
  val number = c_app' "$" "number" []
  val pos = c_app' "$" "pos" []
  val leading_one = c_app' "$" "leading-one" []
  fun bsnoc bs b = c_app' "$" "::" [bs, b]
  val zero = c_app' "$" "zero" []
  fun pos_num n = c_app' "$" "pos_num" [n]

  fun plus n1 n2 n3 = c_app' "$" "plus" [n1, n2, n3]

  val coord = c_app' "$" "coord" []
  fun mk_coord (id, n) = c_app' "$" "mk_coord" [id, n]


  (* Utility stuff *)
  fun intToLFPos 1 = leading_one
    | intToLFPos n =
      if n <= 0 then raise Fail "not a positive number" else
      let val low_bit = if n mod 2 = 0 then b0 else b1
          val other_bits = intToLFPos (n div 2)
      in bsnoc other_bits low_bit end
  fun intToLFNumber 0 = zero
    | intToLFNumber n = pos_num (intToLFPos n)

  (* Turn a closed LF $.bit into an int *)
  fun lfBitToInt (EApp (HConst (LId "$", "b0"), SNil)) = 0
    | lfBitToInt (EApp (HConst (LId "$", "b1"), SNil)) = 1
    | lfBitToInt _ = raise Fail "not a closed $.bit"
  (* Turn a closed LF $.pos into an int *)
  fun lfPosToInt (EApp (HConst (LId "$", "leading-one"), SNil)) = 1
    | lfPosToInt (EApp (HConst (LId "$", "::"), SApp (bs, SApp (b, SNil)))) =
      lfPosToInt bs * 2 + lfBitToInt b
    | lfPosToInt _ = raise Fail "not a closed $.pos"
  (* Turn a closed LF $.number into an int *)
  fun lfNumToInt (EApp (HConst (LId "$", "zero"), SNil)) = 0
    | lfNumToInt (EApp (HConst (LId "$", "pos_num"), SApp (n, SNil))) = lfPosToInt n
    | lfNumToInt _ = raise Fail "not a closed $.number"


  fun byteToLFByte b = c_app' "$" ("b" ^ byteFmt b) []

  (* FIXME: Maybe we should do some checking and produce a better exception. *)
  fun lfByteToByte (EApp (HConst (LId "$", s), SNil)) =
      valOf (Word8.fromString ("0x" ^ String.substring (s, 1, 2)))
    | lfByteToByte _ = raise Fail "not a closed $.byte"

  (* We should probably check that it is actually one of the mk_hash
   * constants but I don't care. Use with care. *)
  fun lfHashToBytestring (EApp (HConst (LId "$", c), s)) =
      let val bytes = map lfByteToByte (spineToList s)
      in Bytestring.implode bytes end
    | lfHashToBytestring _ = raise Fail "not a closed $.hashNNN"

  fun lfCoordToCoord (EApp (HConst (LId "$", "mk_coord"), SApp (s, SApp (i, SNil)))) =
      (lfHashToBytestring s, lfNumToInt i)
    | lfCoordToCoord _ = raise Fail "not a closed $.coord"

  fun lfPrincipalToBytestring (EApp (HConst (LId "$", "principal_hash"), SApp (s, SNil))) =
      lfHashToBytestring s
    | lfPrincipalToBytestring _ = raise Fail "not a closed $.principal"
  fun lfAddressToBytestring (EApp (HConst (LId "$", "address_hash"), SApp (s, SNil))) =
      lfHashToBytestring s
    | lfAddressToBytestring _ = raise Fail "not a closed $.address"



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

  val basis_lf = basis_lf
  val basis = basis

  end

end
