signature FROM_NAMED =
sig
  type ctx = LF.binding list

  val convertBasis : LF.basis -> LF.basis
  val convertLogicBasis : Logic.basis -> Logic.basis

  val convertExp : ctx -> LF.exp -> LF.exp
  val convertHead : ctx -> LF.head -> LF.head
  val convertSpine : ctx -> LF.spine -> LF.spine
  val convertProp : ctx -> Logic.prop -> Logic.prop
  val convertAffirmation : ctx -> Logic.signed_affirmation -> Logic.signed_affirmation
  val convertCondition : ctx -> Logic.condition -> Logic.condition
  val convertProof : ctx -> Logic.proof -> Logic.proof

end

(* Do a conversion from named to de bruijn.
 * Both use the same syntax but named variables have ~1 as index. *)
structure FromNamed : FROM_NAMED =
struct
  type ctx = LF.binding list

  local
      open LF Logic TypeCoinTxn
  in

  fun findIndexBy f y l =
      let fun search' _ nil = NONE
            | search' i (x::xs) = if f (y, x) then SOME i
                                  else search' (i+1) xs
      in search' 0 l end
  fun findIndex y l = findIndexBy (op =) y l

  fun convertHead G (HVar (~1, "_")) = raise Fail "stop that."
    | convertHead G (HVar (~1, s)) = HVar (valOf (findIndex s G), s)
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

         | PForall (b, t, A) =>
           PForall (b, lfconvert t,
                    convertProp (b :: G) A)
         | PExists (b, t, A) =>
           PExists (b, lfconvert t,
                    convertProp (b :: G) A)

         | PAffirms (k, A) => PAffirms (lfconvert k, convert A)
         | PIf (c, A) => PIf (convertCondition G c, convert A)
         | PReceipt (k, A) =>
           PReceipt (lfconvert k, convert A))
      end


  fun convertAffirmation G {persistent, principal, prop, crypto_sig} =
      {persistent = persistent,
       principal = principal,
       crypto_sig = crypto_sig,
       prop = convertProp G prop}

  fun convertProof G proof =
      let val convert = convertProof G
          val lfconvert = convertExp G
      in
      (case proof of
           MRule c => MRule c
         | MVar v => MVar v
         | MLet (M1, v, M2) => MLet (convert M1, v, convert M2)
         | MBang M => MBang (convert M)
         | MBangLet (M1, v, M2) => MBangLet (convert M1, v, convert M2)
         | MLam (v, A, M) => MLam (v, convertProp G A, convert M)
         | MApp (M1, M2) => MApp (convert M1, convert M2)
         | MTensor (M1, M2) => MTensor (convert M1, convert M2)
         | MTensorLet (M1, v1, v2, M2) => MTensorLet (convert M1, v1, v2, convert M2)
         | MWith (M1, M2) => MWith (convert M1, convert M2)
         | MPi (i, M) => MPi (i, convert M)
         | MInj (i, M, A) => MInj (i, convert M, convertProp G A)
         | MCase (M, v1, M1, v2, M2) => MCase (convert M, v1, convert M1, v2, convert M2)
         | MOne => MOne
         | MAbort (M, A) => MAbort (convert M, convertProp G A)
         | MForallLam (x, t, M) => MForallLam (x, lfconvert t, convertProof (x::G) M)
         | MForallApp (M, t) => MForallApp (convert M, lfconvert t)
         | MPack (t, M, A) => MPack (lfconvert t, convert M, convertProp G A)
         | MUnpack (M1, x, v, M2) => MUnpack (convert M1, x, v, convertProof (x::G) M2)
         | MSayReturn (k, M) => MSayReturn (lfconvert k, convert M)
         | MSayBind (M1, v, M2) => MSayBind (convert M1, v, convert M2)
         | MIfReturn (c, M) => MIfReturn (convertCondition G c, convert M)
         | MIfBind (M1, v, M2) => MIfBind (convert M1, v, convert M2)
         | MIfWeaken (c, M) => MIfWeaken (convertCondition G c, convert M)
         | MIfSay M => MIfSay (convert M)

         | MAffirmation aff => MAffirmation (convertAffirmation G aff)
      )
      end

  fun convertBasis basis =
      map (fn (d, c, e) => (d, c, convertExp [] e)) basis


  fun convertLogicBasisEntry (SRule (i, prop)) = SRule (i, convertProp [] prop)
    | convertLogicBasisEntry (SConst (d, c, e)) = SConst (d, c, convertExp [] e)
  fun convertLogicBasis basis = map convertLogicBasisEntry basis

  fun convertLinearGrant basis = map (convertProp []) basis


  end
end
