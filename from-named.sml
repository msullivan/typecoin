(* Do a conversion from named to de bruijn.
 * Both use the same syntax but named variables have ~1 as index. *)
structure FromNamed =
struct

  local
      open LF Logic TypeCoinTxn
  in

  fun findIndexBy f y l =
      let fun search' _ nil = NONE
            | search' i (x::xs) = if f (y, x) then SOME i
                                  else search' (i+1) xs
      in search' 0 l end
  fun findIndex y l = findIndexBy (op =) y l

  fun convertHead G (HVar (~1, s)) = HVar (valOf (findIndex s G), s)
    | convertHead G (HVar (i, _)) = raise Fail "stop that."
    | convertHead G h = h

  fun convertExp G e =
      (case e of
           EKind => EKind
         | EType => EType
         | EProp => EProp
         | ELam (b, e) => ELam (b, convertExp (b :: G) e)
         | EPi (b, e1, e2) => EPi (b, convertExp G e1, convertExp (b :: G) e2)
         | EApp (h, s) => EApp (convertHead G h, convertSpine G s))
  and convertSpine G s = LF.mapSpine (convertExp G) s

  fun convertCondition G (CBefore e) = CBefore (convertExp G e)
    | convertCondition G (CSpent e) = CSpent (convertExp G e)
    | convertCondition G CTrue = CTrue
    | convertCondition G (CNot c) = CNot (convertCondition G c)
    | convertCondition G (CAnd (c1, c2)) =
      CAnd (convertCondition G c1, convertCondition G c2)

  fun convertProp G prop =
      let val convert = convertProp G
          val lfconvert = convertExp G
      in
      (case prop of
           PAtom P => PAtom (lfconvert P)
         | PBang A => PBang (convert A)
         | PLolli (A, B) => PLolli (convert A, convert B)
         | PTensor (A, B) => PTensor (convert A, convert B)
         | PWith (A, B) => PWith (convert A, convert B)
         | POplus (A, B) => POplus (convert A, convert B)
         | POne => POne
         | PZero => PZero
         | PTop => PTop

         | PForall (b, t, A) =>
           PForall (b, lfconvert t,
                    convertProp (b :: G) A)
         | PExists (b, t, A) =>
           PExists (b, lfconvert t,
                    convertProp (b :: G) A)

         | PAffirms (k, A) =>
           PAffirms (lfconvert k, convert A)
(*         | PConstrained (A, cs) =>
           PConstrained (convert A, map (convertConstraint G) cs)*)
         | PReceipt (k, A) =>
           PReceipt (lfconvert k, convert A))
      end

  fun convertLargeElim G convertProof convertBody proof =
      let val convert = convertProof G
          val lfconvert = convertExp G
          val bconvert = convertBody G
      in
      (case proof of
           LBangLet (M1, v, M2) => LBangLet (convert M1, v, bconvert M2)
         | LTensorLet (M1, v1, v2, M2) => LTensorLet (convert M1, v1, v2, bconvert M2)
         | LCase (M, v1, M1, v2, M2) => LCase (convert M, v1, bconvert M1, v2, bconvert M2)
         | LOneLet (M1, M2) => LOneLet (convert M1, bconvert M2)
         | LUnpack (M1, x, v, M2) => LUnpack (convert M1, x, v, convertBody (x::G) M2)
         | LBind (M1, v, M2) => LBind (convert M1, v, bconvert M2)
      )
      end


  fun convertProof G proof =
      let val convert = convertProof G
          val lfconvert = convertExp G
      in
      (case proof of
           MRule c => MRule c
         | MVar v => MVar v
         | MBang M => MBang (convert M)
         | MLam (v, A, M) => MLam (v, convertProp G A, convert M)
         | MApp (M1, M2) => MApp (convert M1, convert M2)
         | MTensor (M1, M2) => MTensor (convert M1, convert M2)
         | MWith (M1, M2) => MWith (convert M1, convert M2)
         | MPi (i, M) => MPi (i, convert M)
         | MInj (i, M, A) => MInj (i, convert M, convertProp G A)
         | MOne => MOne
         | MAbort (M, A, vs) => MAbort (convert M, convertProp G A, vs)
         | MTop vs => MTop vs
         | MForallLam (x, t, M) => MForallLam (x, lfconvert t, convertProof (x::G) M)
         | MForallApp (M, t) => MForallApp (convert M, lfconvert t)
         | MPack (t, M, A) => MPack (lfconvert t, convert M, convertProp G A)
         | MReturn (k, M) => MReturn (lfconvert k, convert M)
         | MLarge l => MLarge (convertLargeElim G convertProof convertProof l)
      )
      end

  fun convertSg sg =
      map (fn (d, c, e) => (d, c, convertExp [] e)) sg

  fun convertAffirmation {principal, prop, crypto_sig} =
      {principal = principal,
       crypto_sig = crypto_sig,
       prop = convertProp [] prop}

  fun convertLogicSgEntry (SRule (i, prop)) = SRule (i, convertProp [] prop)
    | convertLogicSgEntry (SConst (d, c, e)) = SConst (d, c, convertExp [] e)
    | convertLogicSgEntry (SSignedAffirmation (id, affirmation)) =
      SSignedAffirmation (id, convertAffirmation affirmation)
  fun convertLogicSg sg = map convertLogicSgEntry sg

  fun convertLinearSgEntry (LSResource prop) = LSResource (convertProp [] prop)
    | convertLinearSgEntry (LSSignedAffirmation affirmation) =
      LSSignedAffirmation (convertAffirmation affirmation)
  fun convertLinearSg sg = map convertLinearSgEntry sg


  end
end
