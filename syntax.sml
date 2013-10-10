

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

structure PrettyLF =
struct
  local
      open LF

      structure L = Layout

      val $ = L.str
      val % = L.mayAlign
      val & = L.seq

      val WIDTH = 80
      val fmt = L.tostringex WIDTH
  in

  val look_good_but_be_wrong = true


  fun toLayoutHead (HVar (i, s)) =
      if look_good_but_be_wrong then $s
      else $(s ^ "/" ^ Int.toString i)
    | toLayoutHead (HConst s) = $(Const.toStr s)
  fun toLayoutExp e =
      (case e of
           EKind => $"kind"
         | EType => $"type"
         | EProp => $"prop"
         (* Basing this on the variable being called "_" is a bit bogus. *)
         | EPi ("_", e1, e2) =>
           % [&[toLayoutTyParen e1, $" ->"],
              toLayoutExp e2]

         | EPi (b, e1, e2) =>
           % [piPartLayout b e1, toLayoutExp e2]
         | ELam (b, e) =>
           & [$"\\",
              % [ &[$b, $"."],
                  toLayoutExp e]
             ]
         | EApp (h, SNil) => toLayoutHead h
         | EApp (h, s) =>
           & [toLayoutHead h, $" ",
              toLayoutSpine s])
  and toLayoutSpine s =
      let val layouts = map toLayoutExpParen (spineToList s)
      in % (layouts) end

  and toLayoutExpParen (e as ELam _) = L.paren (toLayoutExp e)
    | toLayoutExpParen (e as EApp (_, SApp _)) = L.paren (toLayoutExp e)
    | toLayoutExpParen e = toLayoutExp e
  and toLayoutTyParen (e as ELam _) = L.paren (toLayoutExp e)
    | toLayoutTyParen (e as EPi _) = L.paren (toLayoutExp e)
    | toLayoutTyParen e = toLayoutExp e

  and piPartLayout b e1 = &[$"pi ", $b, $" : ", toLayoutExp e1, $"."]

  (* A nicer layout for "toplevel" pis *)
  fun toLayoutTop e =
      let fun loop l (e as EPi ("_", _, _)) = %[% (rev l), toLayoutExp e]
            | loop l (EPi (b, e1, e2)) = loop (piPartLayout b e1 :: l) e2
            | loop l e = %[% (rev l), toLayoutExp e]

      in loop [] e end

  fun prettyExp e = L.tostringex WIDTH (toLayoutExp e)



  fun prettyMsg msg e = fmt (&[$msg, toLayoutExp e])
  fun prettyMsg2 msg1 e1 sep msg2 e2 =
      fmt (%[ &[$msg1, toLayoutExp e1, $sep],
              &[$msg2, toLayoutExp e2]])

  fun prettyDecl (_, c, e) = fmt (&[$c, $": ", toLayoutTop e, $"."])

  fun prettySg sg =
      String.concatWith "\n" (map prettyDecl sg)

  end
end

structure Logic =
struct

  structure LF = LFSyntax

  (* atoms are LF expressions with kind Prop *)
  type atom = LF.exp
  (* principals are LF expressions with type "$.principal" ?? *)
  type principal = LF.exp

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

                | PForall of LF.binding * LF.exp * prop
                | PExists of LF.binding * LF.exp * prop

                | PAffirms of principal * prop

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

                 | MForallLam of LF.binding * LF.exp * proof
                 | MForallApp of proof * LF.exp
                 | MPack of LF.exp * proof * prop
                 | MUnpack of proof * LF.binding * var * proof

                 (* and something with affirms whatever *)


  datatype sg_entry = SRule of Const.id * prop
                    | SConst of LFSyntax.sg_entry

end
