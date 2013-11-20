structure PrettyLF =
struct
  local
      open LF

      structure L = Layout

      val $ = L.str
      val % = L.mayAlign
      val %% = L.freeStyleAlign
      val & = L.seq

      val WIDTH = 80
      val fmt = L.tostringex WIDTH
  in

  val look_good_but_be_wrong = true


  fun toLayoutHead (HVar (i, s)) =
      if look_good_but_be_wrong then $s
      else $(s ^ "/" ^ Int.toString i)
    | toLayoutHead (HConst s) = $(Const.toStr s)
  fun toLayoutExp e =
      (case e of
           EKind => $"kind"
         | EType => $"type"
         | EProp => $"prop"
         (* Basing this on the variable being called "_" is a bit bogus. *)
         | EPi ("_", e1, e2) =>
           % [&[toLayoutTyParen e1, $" ->"],
              toLayoutExp e2]

         | EPi (b, e1, e2) =>
           % [piPartLayout b e1, toLayoutExp e2]
         | ELam (b, e) =>
           & [$"\\",
              % [ &[$b, $"."],
                  toLayoutExp e]
             ]
         | EApp (h, SNil) => toLayoutHead h
         | EApp (h, s) =>
           & [toLayoutHead h, $" ",
              toLayoutSpine s])
  and toLayoutSpine s =
      let val layouts = map toLayoutExpParen (spineToList s)
      in %% layouts end

  and toLayoutExpParen (e as ELam _) = L.paren (toLayoutExp e)
    | toLayoutExpParen (e as EApp (_, SApp _)) = L.paren (toLayoutExp e)
    | toLayoutExpParen e = toLayoutExp e
  and toLayoutTyParen (e as ELam _) = L.paren (toLayoutExp e)
    | toLayoutTyParen (e as EPi _) = L.paren (toLayoutExp e)
    | toLayoutTyParen e = toLayoutExp e

  and piPartLayout b e1 = &[$"pi ", $b, $" : ", toLayoutExp e1, $"."]

  (* A nicer layout for "toplevel" pis *)
  (* Another try that doesn't screw over ->s ?? *)
  fun toLayoutTop2 e =
      let fun loop l (e as EPi ("_", e1, e2)) = loop (&[toLayoutTyParen e1, $" ->"] :: l) e2
            | loop l (EPi (b, e1, e2)) = loop (piPartLayout b e1 :: l) e2
            | loop l e = %[%% (rev l), toLayoutExp e]
      in loop [] e end
  (* Actually, let's combine them. *)
  fun toLayoutTop1 e =
      let fun loop l (e as EPi ("_", _, _)) = %[% (rev l), toLayoutTop2 e]
            | loop l (EPi (b, e1, e2)) = loop (piPartLayout b e1 :: l) e2
            | loop l e = %[% (rev l), toLayoutTop2 e]
      in loop [] e end

  val toLayoutTop = toLayoutTop1

  fun prettyExp e = L.tostringex WIDTH (toLayoutTop e)



  fun prettyMsg msg e = fmt (&[$msg, toLayoutExp e])
  fun prettyMsg2 msg1 e1 sep msg2 e2 =
      fmt (%[ &[$msg1, toLayoutExp e1, $sep],
              &[$msg2, toLayoutExp e2]])

  fun prettyDecl (_, c, e) = fmt (&[$c, $": ", toLayoutTop e, $"."])

  fun prettySg sg =
      String.concatWith "\n" (map prettyDecl sg)

  end
end


structure PrettyLogic =
struct
  local
      open Logic

      structure L = Layout

      val $ = L.str
      val % = L.mayAlign
      val %% = L.freeStyleAlign
      val & = L.seq

      val WIDTH = 80
      val fmt = L.tostringex WIDTH
  in

  fun isRightAssoc (PLolli _) = true
    | isRightAssoc _ = false

  fun precedence A =
      (case A of
           PAtom _ => 0
         | POne => 0
         | PZero => 0
         | PReceipt _ => 5
         | PIf _ => 5 (* XXX? *)
         | PBang _ => 10
         | PAffirms _ => 50 (* XXX? *)

         (* tensor vs with? *)
         | PTensor _ => 20
         | PWith _ => 20
         | POplus _ => 30
         | PLolli _ => 40

         | PForall _ => 50
         | PExists _ => 50
      )

  fun paren true l = L.paren l
    | paren false l = l

  fun quantLayout sep x t =
      &[$sep, $x, $" : ", PrettyLF.toLayoutExp t, $"."]

  fun abbrevHash bs =
      let val text = Bytestring.toStringHex (Bytestring.rev bs)
      in String.substring (text, 0, 6) ^ "..." end

  fun toLayoutPrincipal k =
      $ (abbrevHash (TypeCoinBasis.lfPrincipalToBytestring k))
  fun toLayoutAddress k =
      $ (abbrevHash (TypeCoinBasis.lfAddressToBytestring k))
  fun toLayoutNumber n =
      $ (Int.toString (TypeCoinBasis.lfNumToInt n))
  fun toLayoutCoord c =
      let val (a, i) = TypeCoinBasis.lfCoordToCoord c
      in L.tuple [$(abbrevHash a), $(Int.toString i)] end

  fun specialLFLayout f e =
      ((f e)
       handle _ => PrettyLF.toLayoutExp e)

(*
  fun toLayoutConstraint (CBefore n) = &[$"before ", specialLFLayout toLayoutNumber n]
    | toLayoutConstraint (CUnrevoked c) = &[$"unrevoked ", specialLFLayout toLayoutCoord c]
*)
  fun toLayoutCondition c =
      let fun toLayoutConditionParenAnd (c as CAnd _) = L.paren (toLayoutCondition c)
            | toLayoutConditionParenAnd c = toLayoutCondition c
      in
          (case c of
               CBefore n => &[$"before(", specialLFLayout toLayoutNumber n, $")"]
             | CSpent c => &[$"spent(", specialLFLayout toLayoutCoord c, $")"]
             | CTrue => $"T"
             | CNot c => &[$"~", toLayoutConditionParenAnd c]
             | CAnd (c1, c2) =>
               % [&[toLayoutCondition c1, $" &"],
                  toLayoutConditionParenAnd c2])
      end

  fun toLayoutProp A =
      (case A of
           PAtom e => PrettyLF.toLayoutExp e
         | POne => $"1"
         | PZero => $"0"
         | PReceipt (k, A) =>
           &[$"Receipt(",
             % (L.separateRight ([ specialLFLayout toLayoutAddress k, toLayoutProp A ], ",")),
             $")"
            ]

         | PIf (c, A) =>
           &[$"If(",
             % (L.separateRight ([ toLayoutCondition c , toLayoutProp A ], ",")),
             $")"
            ]

         | PBang A' => &[$"!", toLayoutPrefix A A']
         | PAffirms (K, A') => &[$"<", specialLFLayout toLayoutPrincipal K, $">",
                                 toLayoutPrefix A A']
         | PForall (x, t, A') =>
           %%[ quantLayout "!" x t,
               toLayoutPrefix A A' ]
         | PExists (x, t, A') =>
           %%[ quantLayout "?" x t,
               toLayoutPrefix A A' ]

         | PTensor As => toLayoutBinop "*" A As
         | PLolli As => toLayoutBinop "-o" A As
         | PWith As => toLayoutBinop "&" A As
         | POplus As => toLayoutBinop "+" A As


      )

  and toLayoutHelp associate A_outer A =
      let val cmp = if associate then (op >) else (op >=)
          val needParens = cmp (precedence A, precedence A_outer)
      in paren needParens (toLayoutProp A) end

  and toLayoutBinop sep A_outer (A1, A2) =
      let val rightAssoc = isRightAssoc A_outer
      in
          % [&[toLayoutHelp (not rightAssoc) A_outer A1, $(" " ^ sep)],
              toLayoutHelp rightAssoc A_outer A2]
      end

  and toLayoutPrefix A_outer A = toLayoutHelp true A_outer A

  (* Another try that doesn't screw over ->s ?? *)
  fun toLayoutTop2 A =
      let fun loop l (A as PLolli (A1, A2)) =
              loop (&[toLayoutHelp false A A1, $" -o"] :: l) A2
            | loop l A = %[%% (rev l), toLayoutProp A]
      in loop [] A end
  fun toLayoutTop1 A =
      let fun loop l (PForall (x, t, A)) = loop (quantLayout "!" x t :: l) A
            | loop l e = %[%% (rev l), toLayoutTop2 e]
      in loop [] A end

  val toLayoutTop = toLayoutTop1

  (* GRRRRR. Where does this belong. *)
  fun affirmationToProp ({principal, prop, ...} : Logic.signed_affirmation) =
      let val hashed_key = TypeCoinCrypto.hashKey principal
          val lf_hash = TypeCoinBasis.hashBytestringToHashObj hashed_key
      in PAffirms (TypeCoinBasis.principal_hash lf_hash, prop) end


  fun prettyDecl (SRule (c, A)) = fmt (&[$c, $": ", toLayoutTop A , $"."])
    | prettyDecl (SConst d) = PrettyLF.prettyDecl d


  fun prettySg sg =
      String.concatWith "\n" (map prettyDecl sg)

  end

end
