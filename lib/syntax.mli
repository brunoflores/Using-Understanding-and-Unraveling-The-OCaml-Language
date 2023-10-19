type name = Name of string | Int of int

type constant =
  | Constr of { name : name; arity : int }
  | Prim of { name : name; arity : int }

type var = string

type expr =
  | Var of var
  | Const of constant
  | Fun of var * expr
  | App of expr * expr
  | Let of var * expr * expr

val plus : constant
val times : constant
val int : int -> constant
val show_expr : expr -> string
