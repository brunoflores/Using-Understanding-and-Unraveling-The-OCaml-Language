open Unraveling_ocaml.Syntax
open Unraveling_ocaml.Reduce

(*
   The example is:
   (\x. (\x. x+1)*(\x. x-1)) ((\x. x+1) 2)
   8
*)
let example =
  let plus_x n = App (App (Const plus, Var "x"), n) in
  App
    ( Fun
        ( "x",
          App
            ( App (Const times, plus_x (Const (int 1))),
              plus_x (Const (int (-1))) ) ),
      App
        ( Fun ("x", App (App (Const plus, Var "x"), Const (int 1))),
          Const (int 2) ) )

(* Redexes are partial functions from programs to programs. Hence, they can be
   represented as OCaml functions, raising an exception [Reduce] when applied
   to values outside of their domain. *)
exception Reduce

let delta_bin_arith op code = function
  | App
      ( App
          ( (Const (Prim { name = Name _; arity = 2 }) as c),
            Const (Constr { name = Int x; _ }) ),
        Const (Constr { name = Int y; _ }) )
    when c = op ->
      Const (int (code x y))
  | _ -> raise Reduce

let delta_plus = delta_bin_arith (Const plus) ( + )
let delta_timees = delta_bin_arith (Const times) ( * )
let delta_rules = [ delta_plus; delta_timees ]

(* The union of partial function (with priority on the right): *)
let union f g a = try g a with Reduce -> f a

(* The delta-reduction: *)
let delta = List.fold_right union delta_rules (fun _ -> raise Reduce)

let rec subst x v a =
  assert (evaluated v);
  match a with
  | Var y -> if x = y then v else a
  | Fun (y, a') -> if x = y then a else Fun (y, subst x v a')
  | App (a', a'') -> App (subst x v a', subst x v a'')
  | Let (y, a', a'') ->
      if x = y then Let (y, subst x v a', a'')
      else Let (y, subst x v a', subst x v a'')
  | Const c -> Const c

let beta = function
  | App (Fun (x, a), v) when evaluated v -> subst x v a
  | Let (x, v, a) when evaluated v -> subst x v a
  | _ -> raise Reduce

let top_reduction = union beta delta

(* The function [eval] visits the tree top-down. *)
let rec eval =
  let eval_top_reduce a = try eval (top_reduction a) with Reduce -> a in
  function
  | App (a1, a2) ->
      let v1 = eval a1 in
      let v2 = eval a2 in
      eval_top_reduce (App (v1, v2))
  | Let (x, a1, a2) ->
      let v1 = eval a1 in
      eval_top_reduce (Let (x, v1, a2))
  | a -> eval_top_reduce a

(*
let rec eval_step = function
  | App (a1, a2) when not (evaluated a1) -> App (eval_step a1, a2)
  | App (a1, a2) when not (evaluated a2) -> App (a1, eval_step a2)
  | Let (x, a1, a2) when not (evaluated a1) -> Let (x, eval_step a1, a2)
  | a -> top_reduction a
*)

(* Contexts as functions from terms to terms. *)
type context = expr -> expr

let hole : context = fun t -> t
let appL a t = App (t, a)
let appR a t = App (a, t)
let letL x a t = Let (x, t, a)
let ( ** ) e1 (e0, a0) = ((fun a -> e1 (e0 a)), a0)

(* Split a term into a pair of an evaluation context and a term: *)
let rec eval_context : expr -> context * expr = function
  | App (a1, a2) when not (evaluated a1) -> appL a2 ** eval_context a1
  | App (a1, a2) when not (evaluated a2) -> appR a1 ** eval_context a2
  | Let (x, a1, a2) when not (evaluated a1) -> letL x a2 ** eval_context a2
  | a -> (hole, a)

let eval_step a =
  let c, t = eval_context a in
  c (top_reduction t)

let rec eval_steps a = try eval_steps (eval_step a) with Reduce -> a

let _ =
  let e = eval example in
  print_endline @@ show_expr e;
  let e' = eval_steps example in
  print_endline @@ show_expr e'

(* Big-step operational semantics
   In small-step, values are always a subset of programs.
   In some cases, it is simpler to let values differ from programs. *)

(*
exception Undefined_constant of string

let type_of_const c =
  let int3 = tarrow tint (tarrow tint tint) in
  match c.name with
  | Int _ -> tint
  | Name ("+" | "*") -> int3
  | Name n -> raise (Undefined_constant n)
*)
