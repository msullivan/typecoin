

structure Binding =
struct

  type var = int * string
  type const = string
  type binding = string

end

structure LFSyntax =
struct

  type var = Binding.var
  type const = Binding.const
  type binding = Binding.binding


  datatype head = HVar of var
                | HConst of const

  datatype exp = EKind
               | EType
               | EProp (* ?? maybe not. *)
               | EPi of binding * exp * exp
               | ELam of binding * exp
               | EApp of head * spine
       and spine = SNil
                 | SApp of exp * spine

end
structure LF = LFSyntax


structure Logic =
struct

  structure LF = LFSyntax

  (* atoms are LF expressions with kind Prop *)
  type atom = LF.exp
  (* principals are LF expressions with type "Key" ?? *)
  type principal = LF.exp


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


end
