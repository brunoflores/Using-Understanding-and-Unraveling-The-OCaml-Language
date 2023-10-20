open Syntax

let rec evaluated = function Fun (_, _) -> true | u -> partial_application 0 u

and partial_application n = function
  | Const (Prim c) -> c.arity > n
  | Const (Constr _) -> true
  | App (u, v) ->
      let e = evaluated v in
      e && partial_application (n + 1) u
  | _ -> false

(* Redexes are partial functions from programs to programs. Hence, they can be
   represented as OCaml functions, raising an exception [Reduce] when applied
   to values outside of their domain. *)
exception Reduce

let delta_bin_arith (op : expr) (code : int -> int -> int) = function
  | App
      ( App
          ( (Const (Prim { name = Name _; arity = 2 }) as c),
            Const (Constr { name = Int x; _ }) ),
        Const (Constr { name = Int y; _ }) )
    when c = op ->
      Const (int (code x y))
  | _ -> raise Reduce

let delta_plus = delta_bin_arith (Const plus) ( + )
let delta_times = delta_bin_arith (Const times) ( * )
let delta_rules = [ delta_plus; delta_times ]

(* The union of partial function (with priority on the right): *)
let union (f : expr -> expr) (g : expr -> expr) (a : expr) =
  try g a with Reduce -> f a

(* The delta-reduction: *)
let delta = List.fold_right union delta_rules (fun (_e : expr) -> raise Reduce)

(* (\x.a) v --> a[v/x] *)
let rec subst (x : var) (v : expr) (a : expr) : expr =
  let _ = assert (evaluated v) in
  match a with
  | Var y -> if String.equal x y then v else a
  | Fun (y, a') -> if String.equal x y then a else Fun (y, subst x v a')
  | App (a', a'') -> App (subst x v a', subst x v a'')
  | Let (y, a', a'') ->
      if String.equal x y then Let (y, subst x v a', a'')
      else Let (y, subst x v a', subst x v a'')
  | Const c -> Const c

let beta = function
  | App (Fun (x, a), v) when evaluated v -> subst x v a
  | Let (x, v, a) when evaluated v -> subst x v a
  | _ -> raise Reduce

let top_reduction = union beta delta
