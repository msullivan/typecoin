(* We merge together the signatures for the LF and Logic layers
 * because they share a namespace. *)
signature SIGNATURE =
sig
  type sg
  val empty : sg
  val insert : sg -> LF.const -> LF.exp -> sg
  val lookup : sg -> LF.const -> LF.exp

  val insert_rule : sg -> Logic.const -> Logic.prop -> sg
  val lookup_rule : sg -> Logic.const -> Logic.prop
end


structure Signature :> SIGNATURE =
struct
  val SignatureError = Fail

  datatype entry =
           LF of LF.exp
         | Logic of Logic.prop

  type sg = entry ConstDict.dict

  val empty = ConstDict.empty

  fun insert' sg c entry =
      if ConstDict.member sg c then
          raise SignatureError (Const.toStr c ^ " is already in signature")
      else ConstDict.insert sg c entry

  fun insert sg c typ = insert' sg c (LF typ)
  fun insert_rule sg c prop = insert' sg c (Logic prop)

  fun lookup sg c =
      (case ConstDict.find sg c of
           NONE => raise SignatureError (Const.toStr c ^ " not in signature")
         | SOME (Logic _) =>
           raise SignatureError (Const.toStr c ^ " is a rule, not an LF constant")
         | SOME (LF t) => t)
  fun lookup_rule sg c =
      (case ConstDict.find sg c of
           NONE => raise SignatureError (Const.toStr c ^ " not in signature")
         | SOME (LF _) =>
           raise SignatureError (Const.toStr c ^ " is an LF constant, not a rule")
         | SOME (Logic A) => A)

end
