(* We merge together the basises for the LF and Logic layers
 * because they share a namespace. *)
signature BASIS =
sig
  type basis
  val empty : basis
  val insert : basis -> LF.const -> LF.exp -> basis
  val lookup : basis -> LF.const -> LF.exp

  val insert_rule : basis -> Logic.const -> Logic.prop -> basis
  val lookup_rule : basis -> Logic.const -> Logic.prop
end


structure Basis :> BASIS =
struct
  val BasisError = Fail

  datatype entry =
           LF of LF.exp
         | Logic of Logic.prop

  type basis = entry ConstDict.dict

  val empty = ConstDict.empty

  fun insert' basis c entry =
      if ConstDict.member basis c then
          raise BasisError (Const.toStr c ^ " is already in basis")
      else ConstDict.insert basis c entry

  fun insert basis c typ = insert' basis c (LF typ)
  fun insert_rule basis c prop = insert' basis c (Logic prop)

  fun lookup basis c =
      (case ConstDict.find basis c of
           NONE => raise BasisError (Const.toStr c ^ " not in basis")
         | SOME (Logic _) =>
           raise BasisError (Const.toStr c ^ " is a rule, not an LF constant")
         | SOME (LF t) => t)
  fun lookup_rule basis c =
      (case ConstDict.find basis c of
           NONE => raise BasisError (Const.toStr c ^ " not in basis")
         | SOME (LF _) =>
           raise BasisError (Const.toStr c ^ " is an LF constant, not a rule")
         | SOME (Logic A) => A)

end
