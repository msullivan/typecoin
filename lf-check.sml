
signature LF_SUBST =
sig
  val hereditaryReduce : LF.exp -> LF.spine -> LF.exp
  val substExp : int -> LF.exp list -> int -> LF.exp -> LF.exp
  val liftExp : int -> LF.exp -> LF.exp
end


structure Subst :> LF_SUBST =
struct

  open LFSyntax

  (* substXMain skip substs substs_len lift exp

     s = substs, n = substs_len, l = lift, m = skip

     if    |s| = n
     then  return exp[0 .. m-1 . s0[^m] .. sn-1[^m] . ^l+m]
   *)
  fun substExpMain skip substs substs_len lift exp =
      (case exp of
           EKind => EKind
         | EType => EType
         | EProp => EProp
         | ELam (b, e) =>
           let val e' = substExpMain (skip+1) substs substs_len lift e
           in ELam (b, e') end
         | EPi (b, e1, e2) =>
           let val e1' = substExpMain skip substs substs_len lift e1
               val e2' = substExpMain (skip+1) substs substs_len lift e2
           in EPi (b, e1', e2') end
         | EApp (head, spine) =>
           let val spine' = substSpineMain skip substs substs_len lift spine
           in (case head of
                   HConst c => EApp (HConst c, spine')
                 | HVar (i, s) =>
                   if i < skip then
                       EApp (HVar (i, s), spine')
                   else if i < skip+substs_len then
                       let val sub = List.nth (substs, i-skip)
                           val sub' = substExpMain 0 [] 0 skip sub
                       (* I'm *pretty* sure all of the index stuff is done.. *)
                       in hereditaryReduce sub' spine end
                   else
                       EApp (HVar (i-substs_len+lift, s), spine'))
           end
      )
  and substSpineMain skip substs substs_len lift spine =
      (case spine of
           SNil => SNil
         | SApp (exp, spine) =>
           let val exp' = substExpMain skip substs substs_len lift exp
               val spine' = substSpineMain skip substs substs_len lift spine
           in SApp (exp', spine') end)

  and hereditaryReduce head spine =
      let fun getBody (ELam (b, e)) n = getBody e (n+1)
            | getBody e n = (e, n)

           fun getSubsts SNil subs = subs
             | getSubsts (SApp (e, s)) subs = getSubsts s (e :: subs)

           val (body, count) = getBody head 0
           val subs = getSubsts spine []

           val () = if count = length subs then ()
                    else raise Fail "bogus application"

      in substExpMain 0 subs (length subs) 0 body end

  fun substExp skip substs lift exp =
      substExpMain skip substs (length substs) lift exp
  fun liftExp lift exp = substExp 0 [] lift exp
end

structure Context =
struct
  type ctx = LFSyntax.exp list
  val empty = nil
  val length = List.length
  fun sub G n =
      let val t = List.nth (G, n)
      in Subst.liftExp n t end
  fun extend G t = t :: G
end

structure Signature =
struct
  type sg = (LF.const * LF.exp) list
  fun lookup' sg c = List.find (fn (c', _) => c = c') sg
  fun insert sg c typ =
      if isSome (lookup' sg c) then
          raise Fail (c ^ " is already in signature")
      else (c, typ) :: sg
  fun lookup sg c =
      (case lookup' sg c of
           NONE => raise Fail (c ^ "not in signature")
         | SOME (_, t) => t)

end

structure TypeCheckLF =
struct

local
  open LFSyntax
  structure Ctx = Context
in

  exception TypeError of string


  (* XXX: do simple typechecking. *)

  fun requireKind exp =
      if exp = EKind then () else raise TypeError "expected kind"

  fun exprEquality e1 e2 = raise Fail "unimpl"

  fun checkExpr sg ctx exp typ =
      (case exp of
           EKind => raise TypeError "kind is no classifier"
         | EType => requireKind typ
         | EProp => requireKind typ
         | EPi (b, e1, e2) =>
           (checkExpr sg ctx e1 EType;
            checkExpr sg (Ctx.extend ctx e1) e2 typ)

         | ELam (b, e) =>
           let val (t1, t2) =
               (case typ of EPi (_, t1, t2) => (t1, t2)
                          | _ => raise TypeError "lambda must have pi type")
           in checkExpr sg (Ctx.extend ctx t1) e t2 end

         | EApp (h, spine) =>
           let val t = checkHead sg ctx h
               val t' = checkSpine sg ctx t spine
           (* XXX: check atomic *)
           in exprEquality t' typ end)
  and checkHead _ ctx (HVar (n, _)) = Ctx.sub ctx n
    | checkHead sg _ (HConst c) = Signature.lookup sg c
  and checkSpine sg ctx typ SNil = typ
    | checkSpine sg ctx typ (SApp (e, s)) =
      let val (t1, t2) =
               (case typ of EPi (_, t1, t2) => (t1, t2)
                          | _ => raise TypeError "lhs of app must be pi")
          val () = checkExpr sg ctx e t1
          (* We could probably arrange to do one big substitution. *)
          val t2' = Subst.substExp 0 [e] 0 t2
      in checkSpine sg ctx t2' s end

end
end
