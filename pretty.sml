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

  fun prettyExp e = L.tostringex WIDTH (toLayoutExp e)



  fun prettyMsg msg e = fmt (&[$msg, toLayoutExp e])
  fun prettyMsg2 msg1 e1 sep msg2 e2 =
      fmt (%[ &[$msg1, toLayoutExp e1, $sep],
              &[$msg2, toLayoutExp e2]])

  fun prettyDecl (_, c, e) = fmt (&[$c, $": ", toLayoutTop e, $"."])

  fun prettySg sg =
      String.concatWith "\n" (map prettyDecl sg)

  end
end
