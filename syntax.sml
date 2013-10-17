

signature VARIABLE =
sig
  type var
  type t = var
  val toStr : var -> string
  val eq : var * var -> bool
  val compare : var * var -> order
end


structure Variable : VARIABLE =
struct
  type var = string
  type t = var
  fun toStr s = s
  fun eq (v: var, v') = v = v'
  val compare = String.compare
end
structure VarDict = SplayDict(structure Key = Variable)
structure VarSet = SplaySet(structure Elem = Variable)


structure Const =
struct
  type namespace = string
  datatype location = LThis | LId of namespace
  type id = string
  type const = location * id
  type t = const

  fun toStr (LThis, s) = s
    | toStr (LId n, s) = n ^ "." ^ s

  fun eq (x: const, y) = x = y

  (* I *despise* writting comparison functions for datatypes. *)
  fun cmp_location (LThis, LThis) = EQUAL
    | cmp_location (LId s1, LId s2) = String.compare (s1, s2)
    | cmp_location (LThis, LId _) = LESS
    | cmp_location (LId _, LThis) = GREATER
  fun compare ((l1, s1), (l2, s2)) =
      (case cmp_location (l1, l2) of
           EQUAL => String.compare (s1, s2)
         | x => x)

end

structure ConstDict = SplayDict(structure Key = Const)
structure ConstSet = SplaySet(structure Elem = Const)


structure LFSyntax =
struct

  type var = int * string
  type const = Const.const
  type binding = string


  datatype head = HVar of var
                | HConst of const

  datatype exp = EKind
               | EType
               | EProp (* ?? maybe not. *)
               | EPi of binding * exp * exp
               | ELam of binding * exp
               | EApp of head * spine
  (* Should spine just be a list? *)
       and spine = SNil
                 | SApp of exp * spine

  datatype entry_type = SgFamilyDecl | SgObjectDecl
  type sg_entry = entry_type * Const.id * exp

  val listToSpine = foldr SApp SNil
  fun spineToList SNil = nil
    | spineToList (SApp (e, s)) = e :: spineToList s
  (* welp. *)
  fun mapSpine f = listToSpine o map f o spineToList

end
structure LF = LFSyntax

structure Logic =
struct

  structure LF = LFSyntax

  (* atoms are LF expressions with kind Prop *)
  type atom = LF.exp
  (* principals are LF expressions with type "$.principal" *)
  type principal = LF.exp
  (* addresses are LF expressions with type "$.address" *)
  type address = LF.exp

  type const = Const.const
  type var = Variable.var

  datatype prop = PAtom of atom
                | PBang of prop
                | PLolli of prop * prop
                | PTensor of prop * prop
                | PWith of prop * prop
                | POplus of prop * prop
                | POne
                | PZero
                | PTop

                | PForall of LF.binding * LF.exp * prop
                | PExists of LF.binding * LF.exp * prop

                | PAffirms of principal * prop
                (* receipts don't have any rules; they are introduced by
                 * typecoin things *)
                | PReceipt of address * prop


  datatype idx = L | R

  datatype proof = MRule of const
                 | MVar of var
                 | MBang of proof
                 | MBangLet of proof * var * proof
                 | MLam of var * prop * proof
                 | MApp of proof * proof
                 | MTensor of proof * proof
                 | MTensorLet of proof * var * var * proof
                 | MWith of proof * proof
                 | MPi of idx * proof
                 | MInj of idx * proof * prop
                 | MCase of proof * var * proof * var * proof
                 | MOne
                 | MOneLet of proof * proof

                 (* annotated with what parts of the context are consumed. *)
                 | MAbort of proof * prop * var list
                 | MTop of var list

                 | MForallLam of LF.binding * LF.exp * proof
                 | MForallApp of proof * LF.exp
                 | MPack of LF.exp * proof * prop
                 | MUnpack of proof * LF.binding * var * proof

                 (* and affirmation stuff; proof terms are monadic *)
                 | MReturn of principal * proof
                 | MBind of proof * var * proof

  (* ????????????? *)
  type bytestring = Word8Vector.vector
  type crypto_sig = bytestring
  type crypto_address = bytestring
  type crypto_principal = bytestring

  type signed_affirmation =
       {principal: crypto_principal,
        prop: prop,
        crypto_sig: crypto_sig}

  datatype sg_entry = SRule of Const.id * prop
                    | SConst of LFSyntax.sg_entry
                    | SSignedAffirmation of Const.id * signed_affirmation


end

structure TypeCoinTxn =
struct
  structure TxnDict = StringSplayDict

  type txnid = Const.namespace

  type crypto_sig = Logic.crypto_sig
  type crypto_address = Logic.crypto_address
  type crypto_principal = Logic.crypto_principal

  datatype input = Input of
           {source: txnid * int,
            prop: Logic.prop
           }
  type inputs = input list

  datatype output = Output of
           {dest: crypto_address,
            prop: Logic.prop
           }
  type outputs = output list

  type persistent_sg = Logic.sg_entry list

  datatype linear_sg_entry =
           LSResource of Logic.prop
         | LSSignedAffirmation of Logic.signed_affirmation
  type linear_sg = linear_sg_entry list


  datatype txn_body = TxnBody of
           {inputs: inputs,
            persistent_sg: persistent_sg,
            linear_sg: linear_sg,
            outputs: outputs,
            proof_term: Logic.proof}
  type txn = txnid * txn_body

  type chain = txn list

end
