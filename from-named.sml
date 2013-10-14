(* Do a conversion from named to de bruijn.
 * Both use the same syntax but named variables have ~1 as index. *)
structure FromNamed =
struct

  local
      open LF Logic
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

         | PAffirms (k, A) =>
           PAffirms (lfconvert k, convert A))
      end

  fun convertProof G proof =
      let val convert = convertProof G
          val lfconvert = convertExp G
      in
      (case proof of
           MRule c => MRule c
         | MVar v => MVar v
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
         | MOneLet (M1, M2) => MOneLet (convert M1, convert M2)
         | MAbort (M, A, vs) => MAbort (convert M, convertProp G A, vs)
         | MForallLam (x, t, M) => MForallLam (x, lfconvert t, convertProof (x::G) M)
         | MForallApp (M, t) => MForallApp (convert M, lfconvert t)
         | MPack (t, M, A) => MPack (lfconvert t, convert M, convertProp G A)
         | MUnpack (M1, x, v, M2) => MUnpack (convert M1, x, v, convertProof (x::G) M2)
         | MReturn (k, M) => MReturn (lfconvert k, convert M)
         | MBind (M1, v, M2) => MBind (convert M1, v, convert M2)
      )
      end


  fun convertSg sg =
      map (fn (d, c, e) => (d, c, convertExp [] e)) sg

  end
end
