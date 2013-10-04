
structure LFTests =
struct
  open LF
  val T = SgFamilyDecl
  val O = SgObjectDecl


  fun arrow t1 t2 =
      EPi ("_", t1, Subst.liftExp 1 t2)


  fun c_app c ls = EApp (HConst c, listToSpine ls)

  val nat = c_app "nat" []
  val zero = c_app "z" []
  fun succ n = c_app "s" [n]
  fun plus n1 n2 n3 = c_app "plus" [n1, n2, n3]

  fun v s = HVar (~1, s)
  fun var s = EApp (v s, SNil)

  val [n, m, q] = map var ["n", "m", "q"]

  val a_test = FromNamed.convertSg
      [(T, "nat", EType),
       (O, "z", nat),
       (O, "s", arrow nat nat),

       (T, "plus", arrow nat (arrow nat (arrow nat EType))),
       (O, "plus/0",
        EPi ("n", nat, plus zero n zero)),
       (O, "plus/s",
        EPi ("m", nat, EPi ("n", nat, EPi ("q", nat,
             arrow (plus n m q)
                   (plus (succ n) m (succ q))))))


      ]

  fun println s = print (s ^ "\n")

  fun check sg =
      (println "";
       println (PrettyLF.prettySg sg);
       ignore (TypeCheckLF.checkSignature sg);
       println "")
      handle (e as TypeCheckLF.TypeError s) => println s




end
