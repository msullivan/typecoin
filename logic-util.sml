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

  val getVariables : ctx -> Variable.var list
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

          fun substCondition (CBefore e) = CBefore (lfsubst e)
            | substCondition (CSpent e) = CSpent (lfsubst e)
            | substCondition CTrue = CTrue
            | substCondition (CNot c) = CNot (substCondition c)
            | substCondition (CAnd (c1, c2)) = CAnd (substCondition c1, substCondition c2)
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
                    substPropMain (skip+1) substs lift loc' loc A)
         | PExists (b, t, A) =>
           PExists (b, lfsubst t,
                    substPropMain (skip+1) substs lift loc' loc A)

         | PIf (c, A) => PIf (substCondition c, subst A)

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

  fun getVariables (_, ctx) = VarDict.domain ctx

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

