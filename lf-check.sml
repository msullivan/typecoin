signature TYPE_CHECK_LF =
sig
  exception TypeError of string

  val expEquality : LF.exp -> LF.exp -> unit
  val checkExp : Signature.sg -> LFContext.ctx -> LF.exp -> LF.exp -> unit

  val thawedType : LF.exp -> unit
  val checkSgEntry : Signature.sg -> LF.sg_entry -> Signature.sg

  val checkSignature : LF.sg -> Signature.sg

end


structure TypeCheckLF : TYPE_CHECK_LF =
struct

local
  open LFSyntax
  structure Ctx = LFContext
  structure Sig = Signature
in

  exception TypeError of string


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

  fun expEquality e e' =
      (case (e, e') of
           (EKind, EKind) => ()
         | (EType, EType) => ()
         | (EProp, EProp) => ()
         | (EPi (_, e1, e2), EPi (_, e1', e2')) =>
           (expEquality e1 e1'; expEquality e2 e2')
         | (ELam (_, e), ELam (_, e')) =>
           expEquality e e'
         | (EApp (h, s), EApp (h', s')) =>
           (headEquality h h'; spineEquality s s')
         | _ => raise TypeError "exps not equal")
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
           (expEquality e e'; spineEquality s s')
         | _ => raise TypeError "spine length mismatch??")


  fun expEquality' e t t' =
      ((expEquality t t')
       handle TypeError s => raise TypeError (
          "equality failure: " ^ s ^ "\n" ^
          PrettyLF.prettyMsg2 "expected: " t ","  "got: " t' ^ "\n" ^
          PrettyLF.prettyMsg "while checking: " e))

  fun checkExp sg ctx exp typ =
      ((*print (PrettyLF.prettyMsg2 "checking: " exp "," "at: " typ ^ "\n");*)
       case exp of
           EKind => raise TypeError "kind is no classifier"
         | EType => requireKind typ
         | EProp => requireKind typ
         | EPi (b, e1, e2) =>
           (checkExp sg ctx e1 EType;
            checkExp sg (Ctx.extend ctx e1) e2 typ)

         | ELam (b, e) =>
           let val (t1, t2) =
               (case typ of EPi (_, t1, t2) => (t1, t2)
                          | _ => raise TypeError "lambda must have pi type")
           in checkExp sg (Ctx.extend ctx t1) e t2 end

         | EApp (h, spine) =>
           let val t = checkHead sg ctx h
               (*val () = print (PrettyLF.prettyMsg "head has type: " t ^ "\n")*)
               val t' = checkSpine sg ctx t spine
               val () = requireAtomic t'
           in expEquality' exp typ t' end)
  and checkHead _ ctx (HVar (n, _)) = Ctx.sub ctx n
    | checkHead sg _ (HConst c) = Sig.lookup sg c
  and checkSpine sg ctx typ SNil = typ
    | checkSpine sg ctx typ (SApp (e, s)) =
      let (*val () = print (PrettyLF.prettyMsg "checking at: " typ ^ "\n")*)
          val (t1, t2) =
               (case typ of EPi (_, t1, t2) => (t1, t2)
                          | _ => raise TypeError "lhs of app must be pi")
          val () = checkExp sg ctx e t1
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
          val () = checkExp sg Ctx.empty exp classifier
          val () = (case entry_type of SgFamilyDecl => ()
                                     | SgObjectDecl => thawedType exp)
      in Sig.insert sg (Const.LThis, c) exp end

  fun checkSignature decls =
      foldl (fn (e, sg) => checkSgEntry sg e) Sig.empty decls

end
end
