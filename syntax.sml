

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
  (* numbers are LF expressions with type "$.number" *)
  type number = LF.exp
  (* coords are LF expressions with type "$.coord" *)
  type coord = LF.exp

  type const = Const.const
  type var = Variable.var

  datatype constraint = CBefore of number
                      | CUnrevoked of coord

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

                | PConstrained of prop * constraint list

                (* receipts don't have any rules; they are introduced by
                 * typecoin things *)
                | PReceipt of address * prop


  datatype idx = L | R

  (* This is a bit annoying. We have to parameterize over 'proof and 'body,
   * even though 'proof will always be proof, because interactions between
   * the lack of polymorphic recursion and the mutual dependency between
   * large_elim and proof. *)
  datatype ('proof, 'body) large_elim =
           LTensorLet of 'proof * var * var * 'body
         | LBangLet of 'proof * var * 'body
         | LOneLet of 'proof * 'body
         | LCase of 'proof * var * 'body * var * 'body
         (* Unpack is a bit funny inside expressions,
          * because it means that our constraints contain variables,
          * which would be bad.
          * Eh, that's fine, though. It just fails the constraint check.
          *)
         | LUnpack of 'proof * LF.binding * var * 'body
         | LBind of 'proof * var * 'body


  datatype proof = MRule of const
                 | MVar of var
                 | MBang of proof
                 | MLam of var * prop * proof
                 | MApp of proof * proof
                 | MTensor of proof * proof
                 | MWith of proof * proof
                 | MPi of idx * proof
                 | MInj of idx * proof * prop
                 | MOne

                 (* annotated with what parts of the context are consumed. *)
                 | MAbort of proof * prop * var list
                 | MTop of var list

                 | MForallLam of LF.binding * LF.exp * proof
                 | MForallApp of proof * LF.exp
                 | MPack of LF.exp * proof * prop

                 (* and affirmation stuff; proof terms are monadic *)
                 | MReturn of principal * proof
                 (* All large elims packed up in an MLarge. *)
                 | MLarge of (proof, proof) large_elim

  val MTensorLet = MLarge o LTensorLet
  val MBangLet = MLarge o LBangLet
  val MOneLet = MLarge o LOneLet
  val MCase = MLarge o LCase
  val MUnpack = MLarge o LUnpack
  val MBind = MLarge o LBind

  (* Proof expressions for the top level thing *)
  datatype pexp =
         (* Include regular proofs *)
           ERet of proof
         (* A sequencing operation for pexps. *)
         | ELet of pexp * var * pexp

         | EOpen of proof

         (* Inclusion of all the large elim forms. *)
         | ELarge of (proof, pexp) large_elim

  val ETensorLet = ELarge o LTensorLet
  val EBangLet = ELarge o LBangLet
  val EOneLet = ELarge o LOneLet
  val ECase = ELarge o LCase
  val EUnpack = ELarge o LUnpack
  val EBind = ELarge o LBind

  (* ????????????? *)
  type bytestring = Word8Vector.vector
  type crypto_sig = bytestring
  type crypto_address = bytestring
  (* Here the principal is the entire public key. *)
  type crypto_principal = bytestring


  datatype real_constraint = RCBefore of int
                           | RCUnrevoked of bytestring * int

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

  type amount = IntInf.int

  datatype input = Input of
           {source: txnid * int,
            prop: Logic.prop
           }
  type inputs = input list

  datatype output = Output of
           {dest: crypto_address,
            prop: Logic.prop,
            needs_receipt: bool,
            amount: amount option
           }
  type outputs = output list

  type persistent_sg = Logic.sg_entry list

  datatype linear_sg_entry =
           LSResource of Logic.prop
         | LSSignedAffirmation of Logic.signed_affirmation
  type linear_sg = linear_sg_entry list


  datatype txn_body = TxnBody of
           {name: string,
            metadata: string list,
            inputs: inputs,
            persistent_sg: persistent_sg,
            linear_sg: linear_sg,
            outputs: outputs,
            var: Variable.var,
            proof_exp: Logic.pexp}
  type txn = txnid * txn_body

  type chain = txn list

end
