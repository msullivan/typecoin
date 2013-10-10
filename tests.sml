
structure LFTests =
struct
  open LF
  val T = SgFamilyDecl
  val O = SgObjectDecl

  (* This depends on the bullshit we are doing. *)
  fun arrow t1 t2 = EPi ("_", t1, t2)
  fun arrow' (t1, t2) = arrow t1 t2
  infixr -->
  val (op -->) = arrow'


  fun c_app c ls = EApp (HConst (Const.LThis, c), listToSpine ls)
  fun c_app' ns c ls = EApp (HConst (Const.LId ns, c), listToSpine ls)

  val nat = c_app "nat" []
  val zero = c_app "z" []
  fun succ n = c_app "s" [n]
  fun plus n1 n2 n3 = c_app "plus" [n1, n2, n3]

  fun v s = HVar (~1, s)
  fun var s = EApp (v s, SNil)

  val [n, m, p, A, B, e, e', D] =
      map var ["n", "m", "p", "A", "B", "e", "e'", "D"]

  val convertSg = FromNamed.convertSg

  val a_test = convertSg
      [(T, "nat", EType),
       (O, "z", nat),
       (O, "s", arrow nat nat),

       (T, "plus", arrow nat (arrow nat (arrow nat EType))),
       (O, "plus/0",
        EPi ("n", nat, plus zero n n)),
       (O, "plus/s",
        EPi ("m", nat, EPi ("n", nat, EPi ("p", nat,
             arrow (plus m n p)
                   (plus (succ m) n (succ p)))))),

       (T, "commutes",
        EPi ("m", nat, EPi ("n", nat, EPi ("p", nat,
             arrow (plus m n p)
             (arrow
              (plus n m p)
              EType))))),

       (T, "0/commutes",
        EPi ("n", nat, plus n zero n --> EType)),
       (O, "-0",
        c_app "0/commutes"
        [zero,
         c_app "plus/0" [zero]]),
       (O, "-1",
        EPi ("n", nat, EPi ("D", plus n zero n,
             c_app "0/commutes" [n, D] -->
             c_app "0/commutes"
             [succ n,
              c_app "plus/s" [n, zero, n, D]])))
      ]

  val tp = c_app "tp" []
  val term = c_app "term" []
  val base = c_app "base" []
  fun arr t1 t2 = c_app "arr" [t1, t2]
  fun eapp t1 t2 = c_app "app" [t1, t2]
  fun eof e A = c_app "of" [e, A]

  val lambda_test = convertSg
      [(T, "tp", EType),
       (O, "base", tp),
       (O, "arr", tp --> tp --> tp),

       (T, "term", EType),
       (O, "app", term --> term --> term),
       (O, "lam", tp --> (term --> term) --> term),

       (T, "of", term --> tp --> EType),
       (O, "of/app",
        EPi ("A", tp, EPi ("B", tp, EPi ("e", term, EPi ("e'", term,
             eof e (arr A B) --> eof e' A --> eof (eapp e e') B)))))

      ]

  (**** "The Basis" ****)
  val nibble' = c_app "nibble" []
  val hash160' = c_app "hash160" []
  val principal' = c_app "principal" []
  val address' = c_app "address" []
  fun makeArrow 0 = hash160'
    | makeArrow n = nibble' --> makeArrow (n-1)
  val basis_test = convertSg
      [(T, "nibble", EType)] @
      List.tabulate (16, fn i => (O, "n"^Int.fmt StringCvt.HEX i, nibble')) @
      [(T, "hash160", EType),
       (O, "hash160_", makeArrow 40),
       (T, "principal", EType),
       (O, "principal_hash", hash160' --> principal'),
       (T, "address", EType),
       (O, "address_hash", hash160' --> address')
      ]

  (* convert a string containing a hash to an LF object of type hash160 *)
  fun hashStringToHashObj s =
      c_app "hash160_"
      (map (fn c => c_app ("n" ^ str (Char.toUpper c)) []) (explode s))
  val test_hash = "349823092af349823092af349823092afbde3434"

  (***********************************)

  fun println s = print (s ^ "\n")

  fun check sg =
      (println "";
       println (PrettyLF.prettySg sg);
       ignore (TypeCheckLF.checkSignature sg);
       println "")
      handle (e as TypeCheckLF.TypeError s) => (println s; raise e)




end
