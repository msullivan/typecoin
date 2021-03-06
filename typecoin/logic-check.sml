signature LOGIC_CHECK =
sig
  exception ProofError of string


  val propEquality : Logic.prop -> Logic.prop -> unit
  val checkProp : Basis.basis -> LogicContext.ctx -> Logic.prop -> unit

  (* Bad name. *)
  val inferProofOuter : Logic.bytestring -> Basis.basis -> LogicContext.ctx -> Logic.proof ->
                        Logic.prop

  val thawedProp : Logic.prop -> unit

  val checkBasis : Basis.basis -> Logic.basis -> Basis.basis
  val installBasis : Basis.basis -> Const.namespace -> Logic.basis -> Basis.basis

  val stdlib_basis : Basis.basis

  (* Hm. Maybe not. *)
  val affirmationToProp : Logic.signed_affirmation -> Logic.prop

end



structure LogicCheck : LOGIC_CHECK =
struct

  local
      open Logic LF
      structure Ctx = LogicContext
      structure Var = Variable
  in

  exception ProofError of string

  fun checkCondition basis ctx c =
      let val check = checkCondition basis ctx
          val checkLF = TypeCheckLF.checkExp basis (Ctx.lfContext ctx)
      in
      (case c of
           CBefore e => checkLF e TypeCoinStdlib.number
         | CSpent e => checkLF e TypeCoinStdlib.coord
         | CTrue => ()
         | CNot c => check c
         | CAnd (c1, c2) => (check c1; check c2)
      )
      end

  fun checkProp basis ctx prop =
      let val check = checkProp basis ctx
          val checkLF = TypeCheckLF.checkExp basis (Ctx.lfContext ctx)

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

         | PForall (b, t, A) =>
           (checkLF t LF.EType;
            checkProp basis (Ctx.extendLF ctx t) A)
         | PExists (b, t, A) =>
           (checkLF t LF.EType;
            checkProp basis (Ctx.extendLF ctx t) A)
         | PIf (c, A) =>
           (check A;
            checkCondition basis ctx c)
         | PAffirms (k, A) =>
           (checkLF k TypeCoinStdlib.principal;
            check A)
         | PReceipt (k, A) =>
           (checkLF k TypeCoinStdlib.address;
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
         | PIf (_, A) => thawedProp A

         (* Not totally sure about whether we want to permit these. *)
         | POplus (A, B) => (thawedProp A; thawedProp B)
         | PExists (_, t, A) => (TypeCheckLF.thawedType t; thawedProp A)
         (* These are really silly, but they aren't wrong or anything.... *)
         | POne => ()

         | PZero => raise Frozen "can't introduce rule for zero!"
         | PAffirms _ => raise Frozen "can't introduce rule for an affirmation!"
         | PReceipt _ => raise Frozen "can't introduce rule for a receipt!")


  val mismatched_props_debug = ref (POne, POne)

  fun conditionEquality c c' =
      (case (c, c') of
           (CTrue, CTrue) => ()
         | (CBefore t, CBefore t') => TypeCheckLF.expEquality t t'
         | (CSpent t, CSpent t') => TypeCheckLF.expEquality t t'
         | (CNot c, CNot c') => conditionEquality c c'
         | (CAnd (c1, c2), CAnd (c1', c2')) =>
           (conditionEquality c1 c1'; conditionEquality c2 c2')
         | _ => raise ProofError "conditions don't match")

  (* This isn't optimized or anything *)
  fun condSearch lAtoms rAtoms (l::ls) rs =
      (case l of
           CTrue => condSearch lAtoms rAtoms ls rs
         | CNot c => condSearch lAtoms rAtoms ls (c::rs)
         | CAnd (c1, c2) => condSearch lAtoms rAtoms (c1::c2::ls) rs
         | c => condSearch (c::lAtoms) rAtoms ls rs)
    | condSearch lAtoms rAtoms [] (r::rs) =
      (case r of
           CTrue => true
         | CNot c => condSearch lAtoms rAtoms [c] rs
         | CAnd (c1, c2) => condSearch lAtoms rAtoms [] (c1::rs) andalso
                            condSearch lAtoms rAtoms [] (c2::rs)
         | c => condSearch lAtoms (c::rAtoms) [] rs)
    | condSearch lAtoms rAtoms [] [] =
      let fun lfIsEqual t1 t2 = (TypeCheckLF.expEquality t1 t2; true)
                                handle _ => false

          fun timeLessThan t1 t2 =
              (let val (n1, n2) = (TypeCoinStdlib.lfNumToInt t1, TypeCoinStdlib.lfNumToInt t2)
               in n1 <= n2 end)
              handle _ => false

          fun atomEntails (CSpent t1) (CSpent t2) = lfIsEqual t1 t2
            | atomEntails (CBefore t1) (CBefore t2) =
              lfIsEqual t1 t2 orelse timeLessThan t1 t2
            | atomEntails _ _ = false

          fun contextEntailsAtom r = List.exists (fn x => atomEntails x r) lAtoms
      in List.exists contextEntailsAtom rAtoms end

  fun conditionEntails cs cs' =
      if condSearch [] [] cs cs' then ()
      else raise ProofError "condition entailment failed"

  (* should we catch TypeErrors and raise proof errors? *)
  fun propEquality A A' =
      (case (A, A') of
           (PAtom t, PAtom t') => TypeCheckLF.expEquality t t'
         | (PBang A, PBang A') => propEquality A A'
         | (PLolli (A1, A2), PLolli (A1', A2')) => (propEquality A1 A1'; propEquality A2 A2')
         | (PTensor (A1, A2), PTensor (A1', A2')) => (propEquality A1 A1'; propEquality A2 A2')
         | (PWith (A1, A2), PWith (A1', A2')) => (propEquality A1 A1'; propEquality A2 A2')
         | (POplus (A1, A2), POplus (A1', A2')) => (propEquality A1 A1'; propEquality A2 A2')
         | (POne, POne) => ()
         | (PZero, PZero) => ()
         | (PForall (_, t, A), PForall (_, t', A'))  =>
           (propEquality A A'; TypeCheckLF.expEquality t t')
         | (PExists (_, t, A), PExists (_, t', A'))  =>
           (propEquality A A'; TypeCheckLF.expEquality t t')
         | (PAffirms (t, A), PAffirms (t', A'))  =>
           (propEquality A A'; TypeCheckLF.expEquality t t')
         | (PReceipt (t, A), PReceipt (t', A'))  =>
           (propEquality A A'; TypeCheckLF.expEquality t t')
         | (PIf (c, A), PIf (c', A')) =>
           (conditionEquality c c'; propEquality A A')
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

  fun discharge v (A, res) =
      (A, VarSet.remove res v)

  fun projIdx L (x, _) = x
    | projIdx R (_, x) = x


  fun affirmationToProp ({principal, prop, ...} : Logic.signed_affirmation) =
      let val hashed_key = TypeCoinCrypto.hashKey principal
          val lf_hash = TypeCoinStdlib.hashBytestringToHashObj hashed_key
      in PAffirms (TypeCoinStdlib.principal_hash lf_hash, prop) end


  fun checkProof T basis (D as (ctx, res)) M =
      let val checkProof = checkProof T basis
          val checkProp = checkProp basis
          val checkLF = TypeCheckLF.checkExp basis (Ctx.lfContext ctx)
          val checkCondition = checkCondition basis ctx
      in
      (case M of
           MRule c => (Basis.lookup_rule basis c, res)
         | MVar v =>
           let val (A, persistent) = Ctx.lookup ctx v
               val res' =
                   if persistent then res else
                   consumeResource res v
           in (A, res') end

         | MLet (M1, v, E2) =>
           let val (A, res') = checkProof D M1
               val D' = addResource (ctx, res') v A
           in checkProof D' E2 end

         | MBang M' =>
           let val (A, empty_res) = checkProof (ctx, VarSet.empty) M'
           in (PBang A, res) end
         | MBangLet (M1, v, E2) =>
           let val (bA1', res') = checkProof D M1
               val A1' = (case bA1' of PBang A1' => A1'
                                     | _ => raise ProofError "let bang of non bang")
               val ctx' = Ctx.insert ctx v A1' true
           in checkProof (ctx', res') E2 end

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
         | MTensorLet (M1, v1, v2, E2) =>
           let val (A', res') = checkProof D M1
               val (A1, A2) =
                   (case A' of PTensor As => As
                             | _ => raise ProofError "tensor let of non tensor")
               val D' = addResource
                            (addResource (ctx, res') v1 A1)
                            v2 A2
           in discharge v1
              (discharge v2
               (checkProof D' E2))
           end

         | MWith (M1, M2) =>
           let val (A1, res1) = checkProof D M1
               val (A2, res2) = checkProof D M2
               val res' = VarSet.intersection res1 res2
           in (PWith (A1, A2), res') end
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
         | MCase (M, v1, E1, v2, E2) =>
           let val (A', res') = checkProof D M
               val (A1, A2) =
                   (case A' of POplus As => As
                             | _ => raise ProofError "case of non oplus")

               val D' = (ctx, res')
               val (C1, res1) = discharge v1
                                (checkProof (addResource D' v1 A1) E1)
               val (C2, res2) = discharge v2
                                (checkProof (addResource D' v2 A2) E2)
               val res' = VarSet.intersection res1 res2
               val () = propEquality C1 C2
           in (C1, res') end

         | MOne => (POne, res)

         | MAbort (M, C) =>
           let val () = checkProp ctx C
               val (A, res') = checkProof D M
               val () = propEquality A PZero
           in (C, res') end

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
         | MUnpack (M1, _, v, E2) =>
           let val (etA, res') = checkProof D M1
               val (_, t, A) =
                   (case etA of PExists x => x
                              | _ => raise ProofError "unpack of non exists")
               val ctx' = Ctx.extendLF ctx t
               val D' = addResource (ctx', res') v A
               val (C, res'') = checkProof D' E2

               (* check that the result prop doesn't mention the variable *)
               val () = checkProp ctx C
           in (C, res'') end


         | MSayReturn (k, M) =>
           let val () = checkLF k TypeCoinStdlib.principal
               val (A, res') = checkProof D M
           in (PAffirms (k, A), res') end
         | MSayBind (M1, v, E2) =>
           let val (affkA, res') = checkProof D M1
               val (k, A) =
                   (case affkA of PAffirms x => x
                                | _ => raise ProofError "bind of non affirms")
               val D' = addResource (ctx, res') v A
               val (affkB, res'') = discharge v (checkProof D' E2)
               val (k', B) =
                   (case affkB of PAffirms x => x
                                | _ => raise ProofError "bind must result in affirms")
               (* principals must match *)
               val () = TypeCheckLF.expEquality k k'
           in (PAffirms (k, B), res'') end

         | MIfReturn (c, M) =>
           let val () = checkCondition c
               val (A, res') = checkProof D M
           in (PIf (c, A), res') end
         | MIfBind (M1, v, E2) =>
           let val (ifcA, res') = checkProof D M1
               val (c, A) =
                   (case ifcA of PIf x => x
                               | _ => raise ProofError "ifbind of non If")
               val D' = addResource (ctx, res') v A
               val (ifcB, res'') = discharge v (checkProof D' E2)
               val (c', B) =
                   (case ifcB of PIf x => x
                               | _ => raise ProofError "ifbind must result in If")
               (* conditions must match *)
               val () = conditionEquality c c'
           in (PIf (c, B), res'') end

         | MIfWeaken (c, M) =>
           let val () = checkCondition c
               val (ifcA, res') = checkProof D M
               val (c', A) =
                   (case ifcA of PIf x => x
                               | _ => raise ProofError "ifweaken of non If")
               val () = conditionEntails [c] [c']
           in (PIf (c, A), res') end
         | MIfSay M =>
           let val (affKifcA, res') = checkProof D M
               val (K, c, A) =
                   (case affKifcA of PAffirms (K, PIf (c, A)) => (K, c, A)
                                   | _ => raise ProofError "bogus if/say")
           in (PIf (c, PAffirms (K, A)), res') end


         | MAffirmation affirmation =>
           let
               val () = if TypeCoinCrypto.checkAffirmation T affirmation then ()
                        else raise ProofError "affirmation signature failure"
               val A = affirmationToProp affirmation
               (* Do we want to allow it to reference things in the context? *)
               val () = checkProp ctx A
           in (A, res) end
      )

      end

  fun inferProofOuter T basis G M =
      let val res = foldl (fn (v, res) => VarSet.insert res v)
                          VarSet.empty (LogicContext.getVariables G)
          val D = (G, res)
          val (A, res) = checkProof T basis D M
      in A end

  fun checkRuleBasisEntry basis (id, prop) =
      (checkProp basis Ctx.empty prop;
       thawedProp prop;
       Basis.insert_rule basis (Const.LThis, id) prop)


  fun checkSignedAffirmationBasisEntry basis (id, affirm) =
      let val prop' = affirmationToProp affirm
          val () = checkProp basis Ctx.empty prop'
      (* crypto is checked in checkCrypto *)
      in Basis.insert_rule basis (Const.LThis, id) prop' end

  fun checkBasisEntry basis (SRule entry) = checkRuleBasisEntry basis entry
    | checkBasisEntry basis (SConst entry) = TypeCheckLF.checkBasisEntry basis entry

  fun checkBasis basis decls =
      foldl (fn (e, basis) => checkBasisEntry basis e) basis decls


  (* Install a declaration in the signature at a certain namespace.
   * Should have already been checked. *)
  fun installBasisEntry basis ns (SRule (id, prop)) =
      let val prop' = LogicSubst.replaceThisProp (Const.LId ns) prop
      in Basis.insert_rule basis (Const.LId ns, id) prop' end
    | installBasisEntry basis ns (SConst (_, id, e)) =
      let val e' = LFSubst.replaceThisExp (Const.LId ns) e
      in Basis.insert basis (Const.LId ns, id) e' end


  fun installBasis basis ns decls =
      foldl (fn (decl, basis) => installBasisEntry basis ns decl) basis decls

  (* Make a basis containing the stdlib *)
  val stdlib_basis =
      (checkBasis Basis.empty TypeCoinStdlib.stdlib;
       installBasis Basis.empty "$" TypeCoinStdlib.stdlib)

end

end
