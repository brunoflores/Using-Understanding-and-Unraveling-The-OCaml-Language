type name = Name of string | Int of int [@@deriving show]

type constant =
  | Constr of { name : name; arity : int }
  | Prim of { name : name; arity : int }
[@@deriving show]

type var = string [@@deriving show]

type expr =
  | Var of var
  | Const of constant
  | Fun of var * expr
  | App of expr * expr
  | Let of var * expr * expr
[@@deriving show]

let plus = Prim { name = Name "+"; arity = 2 }
let times = Prim { name = Name "*"; arity = 2 }
let int n = Constr { name = Int n; arity = 0 }
