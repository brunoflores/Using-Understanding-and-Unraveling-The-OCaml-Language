open Syntax

let rec evaluated = function Fun (_, _) -> true | u -> partial_application 0 u

and partial_application n = function
  | Const (Prim c) -> c.arity > n
  | Const (Constr _) -> true
  | App (u, v) ->
      let e = evaluated v in
      e && partial_application (n + 1) u
  | _ -> false
