
signature LF_SUBST =
sig
  val hereditaryReduce : LF.exp -> LF.spine -> LF.exp
  val substExp : int -> LF.exp list -> int -> LF.exp -> LF.exp
  val liftExp : int -> LF.exp -> LF.exp

  val substAndReplaceExp : int -> LF.exp list -> int -> Const.location -> Const.location
                           -> LF.exp -> LF.exp
  val replaceThisExp : Const.location -> LF.exp -> LF.exp
end


signature CONTEXT =
sig
  type ctx
  val empty : ctx
  val length : ctx -> int
  val sub : ctx -> int -> LF.exp
  val extend : ctx -> LF.exp -> ctx
end

(* We merge together the signatures for the LF and Logic layers
 * because they share a namespace. *)
signature SIGNATURE =
sig
  type sg
  val empty : sg
  val insert : sg -> LF.const -> LF.exp -> sg
  val lookup : sg -> LF.const -> LF.exp

  val insert_rule : sg -> Logic.const -> Logic.prop -> sg
  val lookup_rule : sg -> Logic.const -> Logic.prop
end

structure Signature :> SIGNATURE =
struct
  val SignatureError = Fail

  datatype entry =
           LF of LF.exp
         | Logic of Logic.prop

  type sg = entry ConstDict.dict

  val empty = ConstDict.empty

  fun insert' sg c entry =
      if ConstDict.member sg c then
          raise Fail (Const.toStr c ^ " is already in signature")
      else ConstDict.insert sg c entry

  fun insert sg c typ = insert' sg c (LF typ)
  fun insert_rule sg c prop = insert' sg c (Logic prop)

  fun lookup sg c =
      (case ConstDict.find sg c of
           NONE => raise SignatureError (Const.toStr c ^ " not in signature")
         | SOME (Logic _) =>
           raise SignatureError (Const.toStr c ^ " is a rule, not an LF constant")
         | SOME (LF t) => t)
  fun lookup_rule sg c =
      (case ConstDict.find sg c of
           NONE => raise SignatureError (Const.toStr c ^ " not in signature")
         | SOME (LF _) =>
           raise SignatureError (Const.toStr c ^ " is an LF constant, not a rule")
         | SOME (Logic A) => A)

end



structure LFSubst :> LF_SUBST =
struct

  open LFSyntax

  (* substXMain skip substs substs_len lift ns' ns exp

     s = substs, n = substs_len, l = lift, m = skip

     if    |s| = n
     then  return exp[0 .. m-1 . s0[^m] .. sn-1[^m] . ^l+m][ns'/ns]
   *)
  fun substExpMain skip substs substs_len lift ns' ns exp =
      (case exp of
           EKind => EKind
         | EType => EType
         | EProp => EProp
         | ELam (b, e) =>
           let val e' = substExpMain (skip+1) substs substs_len lift ns' ns e
           in ELam (b, e') end
         | EPi (b, e1, e2) =>
           let val e1' = substExpMain skip substs substs_len lift ns' ns e1
               val e2' = substExpMain (skip+1) substs substs_len lift ns' ns e2
           in EPi (b, e1', e2') end
         | EApp (head, spine) =>
           let val spine' = substSpineMain skip substs substs_len lift ns' ns spine
           in (case head of
                   HConst (loc, c) =>
                   if loc = ns then EApp (HConst (ns', c), spine')
                   else EApp (HConst (loc, c), spine')
                 | HVar (i, s) =>
                   if i < skip then
                       EApp (HVar (i, s), spine')
                   else if i < skip+substs_len then
                       let val sub = List.nth (substs, i-skip)
                           val sub' = substExpMain 0 [] 0 skip ns' ns sub
                       (* I'm *pretty* sure all of the index stuff is done.. *)
                       in hereditaryReduce sub' spine end
                   else
                       EApp (HVar (i-substs_len+lift, s), spine'))
           end
      )
  and substSpineMain skip substs substs_len lift ns' ns spine =
      (case spine of
           SNil => SNil
         | SApp (exp, spine) =>
           let val exp' = substExpMain skip substs substs_len lift ns' ns exp
               val spine' = substSpineMain skip substs substs_len lift ns' ns spine
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

      in substExpMain 0 subs (length subs) 0 Const.LThis Const.LThis body end

  fun substAndReplaceExp skip substs lift loc' loc exp =
      substExpMain skip substs (length substs) lift loc' loc exp
  fun substExp skip substs lift exp =
      substAndReplaceExp skip substs lift Const.LThis Const.LThis exp
  fun liftExp lift exp = substExp 0 [] lift exp
  fun replaceThisExp loc exp = substAndReplaceExp 0 [] 0 loc Const.LThis exp

end

structure LFContext :> CONTEXT =
struct
  type ctx = LFSyntax.exp list
  val empty = nil
  val length = List.length
  fun sub G n =
      let val t = List.nth (G, n)
      in LFSubst.liftExp (n+1) (*XXX?*) t end
  fun extend G t = t :: G
end

structure TypeCheckLF =
struct

local
  open LFSyntax
  structure Ctx = LFContext
  structure Sig = Signature
in

  exception TypeError of string


  (* XXX: do simple typechecking. *)

  fun requireKind exp =
      if exp = EKind then () else raise TypeError "expected kind"
  fun requireAtomic exp =
      (case exp of
           EType => ()
         | EProp => ()
         | EApp _ => ()
         | _ => raise TypeError (PrettyLF.prettyMsg
                                     "required atomic type, got: "
                                     exp))

  fun exprEquality e e' =
      (case (e, e') of
           (EKind, EKind) => ()
         | (EType, EType) => ()
         | (EProp, EProp) => ()
         | (EPi (_, e1, e2), EPi (_, e1', e2')) =>
           (exprEquality e1 e1'; exprEquality e2 e2')
         | (ELam (_, e), ELam (_, e')) =>
           exprEquality e e'
         | (EApp (h, s), EApp (h', s')) =>
           (headEquality h h'; spineEquality s s')
         | _ => raise TypeError "exprs not equal")
  and headEquality h h' =
      (case (h, h') of
           (HVar (i, _), HVar (i', _)) =>
           if i = i' then () else
           raise (TypeError ("bound variable mismatch: " ^
                             Int.toString i ^ " vs. " ^ Int.toString i'))
         | (HConst c, HConst c') =>
           if c = c' then () else
           raise (TypeError ("const mismatch: " ^ Const.toStr c ^ " vs. " ^ Const.toStr c'))
         | _ => raise TypeError "const vs. var mismatch")
  and spineEquality s s' =
      (case (s, s') of
           (SNil, SNil) => ()
         | (SApp (e, s), SApp (e', s')) =>
           (exprEquality e e'; spineEquality s s')
         | _ => raise TypeError "spine length mismatch??")


  fun exprEquality' e t t' =
      ((exprEquality t t')
       handle TypeError s => raise TypeError (
          "equality failure: " ^ s ^ "\n" ^
          PrettyLF.prettyMsg2 "expected: " t ","  "got: " t' ^ "\n" ^
          PrettyLF.prettyMsg "while checking: " e))

  fun checkExpr sg ctx exp typ =
      ((*print (PrettyLF.prettyMsg2 "checking: " exp "," "at: " typ ^ "\n");*)
       case exp of
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
               (*val () = print (PrettyLF.prettyMsg "head has type: " t ^ "\n")*)
               val t' = checkSpine sg ctx t spine
               val () = requireAtomic t'
           in exprEquality' exp typ t' end)
  and checkHead _ ctx (HVar (n, _)) = Ctx.sub ctx n
    | checkHead sg _ (HConst c) = Sig.lookup sg c
  and checkSpine sg ctx typ SNil = typ
    | checkSpine sg ctx typ (SApp (e, s)) =
      let (*val () = print (PrettyLF.prettyMsg "checking at: " typ ^ "\n")*)
          val (t1, t2) =
               (case typ of EPi (_, t1, t2) => (t1, t2)
                          | _ => raise TypeError "lhs of app must be pi")
          val () = checkExpr sg ctx e t1
          (* We could probably arrange to do one big substitution. *)
          val t2' = LFSubst.substExp 0 [e] 0 t2
      in checkSpine sg ctx t2' s end

  (* Check that we aren't creating ways to build things of
   * types declared in other namespaces. *)
  fun thawedType e =
      (case e of
           EPi (_, _, t) => thawedType t
         | EApp (HConst (Const.LThis, _), _) => ()
         | EApp (HConst (Const.LId s, _), _) =>
           raise TypeError ("cannot introduce type from txn " ^ s)
         | _ => raise Fail "bogus type")

  fun checkSgEntry sg ((entry_type, c, exp): LF.sg_entry) =
      let val classifier =
              (case entry_type of SgFamilyDecl => EKind
                                | SgObjectDecl => EType)
          val () = checkExpr sg Ctx.empty exp classifier
          val () = (case entry_type of SgFamilyDecl => ()
                                     | SgObjectDecl => thawedType exp)
      in Sig.insert sg (Const.LThis, c) exp end

  fun checkSignature decls =
      foldl (fn (e, sg) => checkSgEntry sg e) Sig.empty decls

end
end
