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

  fun convertSg sg =
      map (fn (d, c, e) => (d, c, convertExp [] e)) sg

  end
end
