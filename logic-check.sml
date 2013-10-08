

structure LogicSubst(* :> LOGIC_SUBST*) =
struct

  open Logic

  (* substProp skip substs lift A

     s = substs, l = lift, m = skip

     if    |s| = n
     then  return A[0 .. m-1 . s0[^m] .. sn-1[^m] . ^l+m]
   *)
  fun substProp skip substs lift prop =
      let val lfsubst = LFSubst.substExp skip substs lift
          val subst = substProp skip substs lift
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

         | PForall (b, t, A) =>
           PForall (b, lfsubst t,
                    substProp (skip+1) substs lift A)
         | PExists (b, t, A) =>
           PExists (b, lfsubst t,
                    substProp (skip+1) substs lift A)

         | PAffirms (k, A) =>
           PAffirms (lfsubst k, subst A)

      )
      end

  fun liftProp 0 A = A (* optimiz *)
    | liftProp lift A = substProp 0 [] lift A
end


structure LogicContext (*:> CONTEXT*) =
struct
  val varToStr = Logic.varToStr

  datatype entry = E of
           {name: Logic.var,
            prop: Logic.prop,
            lf_ctx_len: int,
            persistent: bool}

  type ctx = LFContext.ctx * entry list
  val empty = (LFContext.empty, nil)

  fun lfContext (lf_ctx, _) = lf_ctx

  fun lookup' ((_, ctx): ctx) v = List.find (fn E {name, ...} => v = name) ctx

  fun length (_, D) = List.length D

  fun insert (ctx as (lf_ctx, D)) v A persistent =
      if isSome (lookup' ctx v) then
          raise Fail (varToStr v ^ " is already in context")
      else (lf_ctx,
            (E {name=v, prop=A, lf_ctx_len=LFContext.length lf_ctx,
                persistent=persistent}) :: D)

  fun extendLF (lf_ctx, logic_ctx) t = (LFContext.extend lf_ctx t, logic_ctx)

  fun lookup (ctx as (lf_ctx, _)) v =
      (case lookup' ctx v of
           NONE => raise Fail (varToStr v ^ " not in context")
         | SOME (E {prop, lf_ctx_len, persistent, ...}) =>
           (LogicSubst.liftProp (LFContext.length lf_ctx - lf_ctx_len) prop,
            persistent))

  (* we want to maintain by invariant uniqueness *)
  type resource_set = Logic.var list

  val emptyResources = nil
  val setIsEmpty = null
  fun addToSet res x = x :: res

  fun containsResource (res: resource_set) x =
      List.exists (fn x' => x = x') res
  (* PERF: stupid *)
  fun removeResource res x =
      List.filter (fn x' => x <> x') res

  fun addResource (ctx, res) x A =
      (insert ctx x A false, addToSet res x)



end


structure LogicCheck =
struct

  local
      open Logic LF
      structure Ctx = LogicContext
  in

  exception ProofError of string

  fun checkProp ctx prop = ()

  fun propEquality (_: prop) (_: prop) = raise Fail "unimplemented"

  fun requireResource res v =
      if Ctx.containsResource res v then () else
      raise ProofError ("missing required resource " ^ varToStr v)
  fun consumeResource res v =
      (requireResource res v; Ctx.removeResource res v)

  fun requireResourceEquality res1 res2 = raise Fail "unimplemented"

  fun discharge v (A, res) =
      (if Ctx.containsResource res v then
           raise ProofError ("resource not used: " ^ varToStr v)
       else ();
       (A, res))

  fun projIdx L (x, _) = x
    | projIdx R (_, x) = x

  fun checkProof sg (D as (ctx, res)) M =
      let val checkProof = checkProof sg
      in
      (case M of
           MRule c => raise Fail "unimpl"
         | MVar v =>
           let val (A, persistent) = Ctx.lookup ctx v
               val res' =
                   if persistent then res else
                   consumeResource res v
           in (A, res') end

         | MBang M' =>
           let val (A, empty_res) = checkProof (ctx, Ctx.emptyResources) M
           in (PBang A, res) end
         | MBangLet (M1, v, M2) =>
           let val (bA1', res') = checkProof D M1
               val A1' = (case bA1' of PBang A1' => A1'
                                     | _ => raise ProofError "let bang of non bang")
               val ctx' = Ctx.insert ctx v A1' true
           in checkProof (ctx', res') M2 end

         | MLam (v, A, M) =>
           let val () = checkProp ctx A
               val D' = Ctx.addResource D v A
           in discharge v (checkProof D' M)
           end
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
               val D' = Ctx.addResource
                            (Ctx.addResource (ctx, res') v1 A1)
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
                                (checkProof (Ctx.addResource D' v1 A1) M1)
               val (C2, res2) = discharge v2
                                (checkProof (Ctx.addResource D' v2 A2) M2)
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

         | MForallLam (b, t, M) =>
           let val () = TypeCheckLF.checkExpr sg (Ctx.lfContext ctx) t LF.EType
               val ctx' = Ctx.extendLF ctx t
               val (A, res') = checkProof (ctx', res) M
           in (PForall (b, t, A), res') end
         | MForallApp (M, e) =>
           let val (faA, res') = checkProof D M
               val (_, t, A) =
                   (case faA of PForall x => x
                              | _ => raise ProofError "fapp of non forall")
               val () = TypeCheckLF.checkExpr sg (Ctx.lfContext ctx) e t
               val A' = LogicSubst.substProp 0 [e] 0 A
           in (A', res) end

         | MPack (e, M, A) =>
           let val () = checkProp ctx A
               val (_, t, A') =
                   (case A of PExists x => x
                            | _ => raise ProofError "pack ann not an exists")
               val () = TypeCheckLF.checkExpr sg (Ctx.lfContext ctx) e t
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
               val D' = Ctx.addResource (ctx', res') v A
               val (C, res'') = checkProof D' M2

               (* check that the result prop doesn't mention the variable *)
               val () = checkProp ctx C
           in (C, res'') end

      )

      end


end

end
