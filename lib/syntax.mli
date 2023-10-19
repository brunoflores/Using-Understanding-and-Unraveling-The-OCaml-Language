type name = Name of string | Int of int

type constant =
  | Constr of { name : name; arity : int }
  | Prim of { name : name; arity : int }

type var = string

(* Abstract syntax tree *)
type expr =
  | Var of var
  | Const of constant
  | Fun of var * expr
  | App of expr * expr
  | Let of var * expr * expr

(* Auxiliary functions to build constants *)
val plus : constant
val times : constant
val int : int -> constant

(* Printing *)
val show_expr : expr -> string
