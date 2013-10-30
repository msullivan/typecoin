
signature LOGIC_SUBST =
sig
  val substProp : int -> LFSyntax.exp list -> int -> Logic.prop -> Logic.prop
  val replaceThisProp : Const.location -> Logic.prop -> Logic.prop
  val replaceLocProp : Const.location -> Const.location -> Logic.prop -> Logic.prop
  val liftProp : int -> Logic.prop -> Logic.prop
end

signature LOGIC_CONTEXT =
sig
  type ctx
  val empty : ctx
  val fromLFContext : LFContext.ctx -> ctx
  val lfContext : ctx -> LFContext.ctx
  val insert : ctx -> Variable.var -> Logic.prop -> bool -> ctx
  val extendLF : ctx -> LFSyntax.exp -> ctx
  val lookup : ctx -> Variable.var -> Logic.prop * bool
end

structure LogicSubst :> LOGIC_SUBST =
struct

  open Logic

  (* substProp skip substs lift loc' loc A

     s = substs, l = lift, m = skip

     if    |s| = n
     then  return A[0 .. m-1 . s0[^m] .. sn-1[^m] . ^l+m][loc'/loc]
   *)
  fun substPropMain skip substs lift loc' loc prop =
      let val lfsubst = LFSubst.substAndReplaceExp skip substs lift loc' loc
          val subst = substPropMain skip substs lift loc' loc
      in
      (case prop of
           PAtom p => PAtom (lfsubst p)
         | PBang A => PBang (subst A)
         | PLolli (A, B) => PLolli (subst A, subst B)
         | PTensor (A, B) => PTensor (subst A, subst B)
         | PWith (A, B) => PWith (subst A, subst B)
         | POplus (A, B) => POplus (subst A, subst B)
         | POne => POne
         | PZero => PZero
         | PTop => PTop

         | PForall (b, t, A) =>
           PForall (b, lfsubst t,
                    substPropMain (skip+1) substs lift loc' loc A)
         | PExists (b, t, A) =>
           PExists (b, lfsubst t,
                    substPropMain (skip+1) substs lift loc' loc A)

         | PAffirms (k, A) =>
           PAffirms (lfsubst k, subst A)
         | PReceipt (k, A) =>
           PReceipt (lfsubst k, subst A)

      )
      end

  fun substProp skip substs lift prop =
      substPropMain skip substs lift Const.LThis Const.LThis prop
  (* [loc'/loc]prop *)
  fun replaceLocProp loc' loc prop =
      substPropMain 0 [] 0 loc' loc prop
  (* [loc/this]prop *)
  fun replaceThisProp loc prop =
      replaceLocProp loc Const.LThis prop

  fun liftProp 0 A = A (* optimiz *)
    | liftProp lift A = substProp 0 [] lift A
end



structure LogicContext :> LOGIC_CONTEXT =
struct
  val varToStr = Variable.toStr

  datatype entry = E of
           {prop: Logic.prop,
            lf_ctx_len: int,
            persistent: bool}

  type ctx = LFContext.ctx * entry VarDict.dict
  val empty = (LFContext.empty, VarDict.empty)

  fun fromLFContext lf_ctx = (lf_ctx, VarDict.empty)

  fun lfContext (lf_ctx, _) = lf_ctx

  fun insert (lf_ctx, logic_ctx) v A persistent =
      if VarDict.member logic_ctx v then
          raise Fail (varToStr v ^ " is already in context")
      else (lf_ctx,
            VarDict.insert logic_ctx v
                           (E {prop=A, lf_ctx_len=LFContext.length lf_ctx,
                               persistent=persistent}))

  fun extendLF (lf_ctx, logic_ctx) t = (LFContext.extend lf_ctx t, logic_ctx)

  fun lookup (lf_ctx, logic_ctx) v =
      (case VarDict.find logic_ctx v of
           NONE => raise Fail (varToStr v ^ " not in context")
         | SOME (E {prop, lf_ctx_len, persistent, ...}) =>
           (LogicSubst.liftProp (LFContext.length lf_ctx - lf_ctx_len) prop,
            persistent))
end

structure LogicCheck =
struct

  local
      open Logic LF
      structure Ctx = LogicContext
      structure Var = Variable
  in

  exception ProofError of string

  val principal_ty = TypeCoinBasis.principal
  val address_ty = TypeCoinBasis.address

  fun checkProp sg ctx prop =
      let val check = checkProp sg ctx
          val checkLF = TypeCheckLF.checkExpr sg (Ctx.lfContext ctx)
      in
      (case prop of
           PAtom t => checkLF t LF.EProp
         | PBang A => check A
         | PLolli (A1, A2) => (check A1; check A2)
         | PTensor (A1, A2) => (check A1; check A2)
         | PWith (A1, A2) => (check A1; check A2)
         | POplus (A1, A2) => (check A1; check A2)
         | POne => ()
         | PZero => ()
         | PTop => ()

         | PForall (b, t, A) =>
           (checkLF t LF.EType;
            checkProp sg (Ctx.extendLF ctx t) A)
         | PExists (b, t, A) =>
           (checkLF t LF.EType;
            checkProp sg (Ctx.extendLF ctx t) A)

         | PAffirms (k, A) =>
           (checkLF k principal_ty;
            check A)
         | PReceipt (k, A) =>
           (checkLF k address_ty;
            check A))
      end

  (* Check that we aren't creating ways to prove things of
   * props declared in other namespaces. *)
  val Frozen = ProofError
  fun thawedProp A =
      (case A of
           PAtom (EApp (HConst (Const.LThis, _), _)) => ()
         | PAtom (EApp (HConst (Const.LId s, _), _)) =>
           raise Frozen ("cannot prove atom from txn " ^ s)
         | PAtom _ => raise Fail "bogus atom"
         | PBang A => thawedProp A
         | PLolli (A, B) => thawedProp B
         | PTensor (A, B) => (thawedProp A; thawedProp B)
         | PWith (A, B) => (thawedProp A; thawedProp B)
         | PForall (_, _, A) => thawedProp A

         (* Not totally sure about whether we want to permit these. *)
         | POplus (A, B) => raise Frozen "oplus not supported"
         | POne => raise Frozen "one is silly"
         | PTop => raise Frozen "top is silly"
         | PExists (_, _, A) => raise Frozen "exists not supported"

         | PZero => raise Frozen "can't introduce rule for zero!"
         | PAffirms _ => raise Frozen "can't introduce rule for an affirmation!"
         | PReceipt _ => raise Frozen "can't introduce rule for a receipt!")


  val mismatched_props_debug = ref (POne, POne)

  (* should we catch TypeErrors and raise proof errors? *)
  fun propEquality A A' =
      (case (A, A') of
           (PAtom t, PAtom t') => TypeCheckLF.exprEquality t t'
         | (PBang A, PBang A') => propEquality A A'
         | (PLolli (A1, A2), PLolli (A1', A2')) => (propEquality A1 A1'; propEquality A2 A2')
         | (PTensor (A1, A2), PTensor (A1', A2')) => (propEquality A1 A1'; propEquality A2 A2')
         | (PWith (A1, A2), PWith (A1', A2')) => (propEquality A1 A1'; propEquality A2 A2')
         | (POplus (A1, A2), POplus (A1', A2')) => (propEquality A1 A1'; propEquality A2 A2')
         | (POne, POne) => ()
         | (PZero, PZero) => ()
         | (PForall (_, t, A), PForall (_, t', A'))  =>
           (propEquality A A'; TypeCheckLF.exprEquality t t')
         | (PExists (_, t, A), PExists (_, t', A'))  =>
           (propEquality A A'; TypeCheckLF.exprEquality t t')
         | (PAffirms (t, A), PAffirms (t', A'))  =>
           (propEquality A A'; TypeCheckLF.exprEquality t t')
         | (PReceipt (t, A), PReceipt (t', A'))  =>
           (propEquality A A'; TypeCheckLF.exprEquality t t')
         | ps => (mismatched_props_debug := ps; raise ProofError "props don't match")
      )

  val mismatched_props_debug2 = ref (POne, POne)
  fun propEquality' A A' = (mismatched_props_debug2 := (A, A'); propEquality A A')
  val propEquality = propEquality'

  fun addResource (ctx, res) x A =
      (Ctx.insert ctx x A false, VarSet.insert res x)

  fun requireResource res v =
      if VarSet.member res v then () else
      raise ProofError ("missing required resource " ^ Var.toStr v)
  fun consumeResource res v =
      (requireResource res v; VarSet.remove res v)

  fun requireResourceEquality res1 res2 =
      if VarSet.eq (res1, res2) then () else
      raise ProofError "used resources don't match"

  fun discharge v (A, res) =
      (if VarSet.member res v then
           raise ProofError ("resource not used: " ^ Var.toStr v)
       else ();
       (A, res))

  fun projIdx L (x, _) = x
    | projIdx R (_, x) = x

  fun checkProof sg (D as (ctx, res)) M =
      let val checkProof = checkProof sg
          val checkProp = checkProp sg
          val checkLF = TypeCheckLF.checkExpr sg (Ctx.lfContext ctx)
      in
      (case M of
           MRule c => (Signature.lookup_rule sg c, res)
         | MVar v =>
           let val (A, persistent) = Ctx.lookup ctx v
               val res' =
                   if persistent then res else
                   consumeResource res v
           in (A, res') end

         | MBang M' =>
           let val (A, empty_res) = checkProof (ctx, VarSet.empty) M'
           in (PBang A, res) end
         | MBangLet (M1, v, M2) =>
           let val (bA1', res') = checkProof D M1
               val A1' = (case bA1' of PBang A1' => A1'
                                     | _ => raise ProofError "let bang of non bang")
               val ctx' = Ctx.insert ctx v A1' true
           in checkProof (ctx', res') M2 end

         | MLam (v, A, M) =>
           let val () = checkProp ctx A
               val D' = addResource D v A
               val (B, res') = discharge v (checkProof D' M)
           in (PLolli (A, B), res') end
         | MApp (M1, M2) =>
           let val (A', res') = checkProof D M1
               val (A1, A2) =
                   (case A' of PLolli As => As
                             | _ => raise ProofError "app of non lolli")
               val (A1', res'') = checkProof (ctx, res') M2
               val () = propEquality A1 A1'
           in (A2, res'') end


         | MTensor (M1, M2) =>
           let val (A1, res') = checkProof (ctx, res) M1
               val (A2, res'') = checkProof (ctx, res') M2
           in (PTensor (A1, A2), res'') end
         | MTensorLet (M1, v1, v2, M2) =>
           let val (A', res') = checkProof D M1
               val (A1, A2) =
                   (case A' of PTensor As => As
                             | _ => raise ProofError "tensor let of non tensor")
               val D' = addResource
                            (addResource (ctx, res') v1 A1)
                            v2 A2
           in discharge v1
              (discharge v2
               (checkProof D' M2))
           end

         | MWith (M1, M2) =>
           let val (A1, res1) = checkProof D M1
               val (A2, res2) = checkProof D M2
               val () = requireResourceEquality res1 res2
           in (PWith (A1, A2), res1) end
         | MPi (idx, M) =>
           let val (A', res') = checkProof D M
               val As =
                   (case A' of PWith As => As
                             | _ => raise ProofError "pi of non with")
           in (projIdx idx As, res') end

         | MInj (idx, M, A) =>
           let val () = checkProp ctx A
               val As =
                   (case A of POplus As => As
                            | _ => raise ProofError "inj ann not an oplus")
               val (A', res') = checkProof D M
               val () = propEquality A' (projIdx idx As)
           in (POplus As, res') end
         | MCase (M, v1, M1, v2, M2) =>
           let val (A', res') = checkProof D M
               val (A1, A2) =
                   (case A' of POplus As => As
                             | _ => raise ProofError "case of non oplus")

               val D' = (ctx, res')
               val (C1, res1) = discharge v1
                                (checkProof (addResource D' v1 A1) M1)
               val (C2, res2) = discharge v2
                                (checkProof (addResource D' v2 A2) M2)
               val () = requireResourceEquality res1 res2
               val () = propEquality C1 C2
           in (C1, res1) end

         | MOne => (POne, res)
         | MOneLet (M1, M2) =>
           let val (A1, res') = checkProof D M1
               val () = propEquality A1 POne
           in checkProof (ctx, res') M2 end

         | MAbort (M, C, consumed) =>
           let val res' = foldl (fn (v, res') => consumeResource res' v) res consumed
               val () = checkProp ctx C
               val (A, res'') = checkProof (ctx, res') M
               val () = propEquality A PZero
           in (C, res'') end
         | MTop consumed =>
           let val res' = foldl (fn (v, res') => consumeResource res' v) res consumed
           in (PTop, res') end


         | MForallLam (b, t, M) =>
           let val () = checkLF t LF.EType
               val ctx' = Ctx.extendLF ctx t
               val (A, res') = checkProof (ctx', res) M
           in (PForall (b, t, A), res') end
         | MForallApp (M, e) =>
           let val (faA, res') = checkProof D M
               val (_, t, A) =
                   (case faA of PForall x => x
                              | _ => raise ProofError "fapp of non forall")
               val () = checkLF e t
               val A' = LogicSubst.substProp 0 [e] 0 A
           in (A', res) end

         | MPack (e, M, A) =>
           let val () = checkProp ctx A
               val (_, t, A') =
                   (case A of PExists x => x
                            | _ => raise ProofError "pack ann not an exists")
               val () = checkLF e t
               val A'' = LogicSubst.substProp 0 [e] 0 A'

               val (A''', res') = checkProof D M
               val () = propEquality A'' A'''
           in (A, res') end
         | MUnpack (M1, _, v, M2) =>
           let val (etA, res') = checkProof D M1
               val (_, t, A) =
                   (case etA of PExists x => x
                              | _ => raise ProofError "unpack of non exists")
               val ctx' = Ctx.extendLF ctx t
               val D' = addResource (ctx', res') v A
               val (C, res'') = checkProof D' M2

               (* check that the result prop doesn't mention the variable *)
               val () = checkProp ctx C
           in (C, res'') end


         | MReturn (k, M) =>
           let val () = checkLF k principal_ty
               val (A, res') = checkProof D M
           in (PAffirms (k, A), res') end
         | MBind (M1, v, M2) =>
           let val (affkA, res') = checkProof D M1
               val (k, A) =
                   (case affkA of PAffirms x => x
                                | _ => raise ProofError "bind of non affirms")
               val D' = addResource (ctx, res') v A
               val (affkB, res'') = discharge v (checkProof D' M2)
               val (k', B) =
                   (case affkB of PAffirms x => x
                                | _ => raise ProofError "bind must result in affirms")
               (* principals must match *)
               val () = TypeCheckLF.exprEquality k k'
           in (PAffirms (k, B), res'') end

      )

      end

      fun inferProofOuter sg M =
          let val D = (LogicContext.empty, VarSet.empty)
              val (A, res) = checkProof sg D M
              val () = if VarSet.isEmpty res then () else
                       raise ProofError "not all resources used"
          in A end

      fun checkRuleSgEntry sg (id, prop) =
          (checkProp sg Ctx.empty prop;
           thawedProp prop;
           Signature.insert_rule sg (Const.LThis, id) prop)

      fun affirmationToProp ({principal, prop, ...} : Logic.signed_affirmation) =
          let val hashed_key = TypeCoinCrypto.hashKey principal
              val lf_hash = TypeCoinBasis.hashBytestringToHashObj hashed_key
          in PAffirms (TypeCoinBasis.principal_hash lf_hash, prop) end

      fun checkSignedAffirmationSgEntry sg (id, affirm) =
          let val prop' = affirmationToProp affirm
              val () = checkProp sg Ctx.empty prop'
              (* crypto is checked in checkCrypto *)
          in Signature.insert_rule sg (Const.LThis, id) prop' end

      fun checkSgEntry sg (SRule entry) = checkRuleSgEntry sg entry
        | checkSgEntry sg (SSignedAffirmation entry) = checkSignedAffirmationSgEntry sg entry
        | checkSgEntry sg (SConst entry) = TypeCheckLF.checkSgEntry sg entry

      fun checkSignature sg decls =
          foldl (fn (e, sg) => checkSgEntry sg e) sg decls


      (* Install a declaration in the signature at a certain namespace.
       * Should have already been checked. *)
      fun installSgEntry sg ns (SRule (id, prop)) =
          let val prop' = LogicSubst.replaceThisProp (Const.LId ns) prop
          in Signature.insert_rule sg (Const.LId ns, id) prop' end
        | installSgEntry sg ns (SConst (_, id, e)) =
          let val e' = LFSubst.replaceThisExp (Const.LId ns) e
          in Signature.insert sg (Const.LId ns, id) e' end
        | installSgEntry sg ns (SSignedAffirmation (id, affirm)) =
          let val prop' = LogicSubst.replaceThisProp (Const.LId ns) (affirmationToProp affirm)
          in Signature.insert_rule sg (Const.LId ns, id) prop' end



     fun installSignature sg ns decls =
         foldl (fn (decl, sg) => installSgEntry sg ns decl) sg decls

     (* Make a signature containing the basis *)
     val basis_sg =
         (checkSignature Signature.empty TypeCoinBasis.basis;
          installSignature Signature.empty "$" TypeCoinBasis.basis)


end

end
