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

(* The function [eval] visits the tree top-down. *)
let rec eval e =
  let eval_top_reduce a = try eval (top_reduction a) with Reduce -> a in
  match e with
  | App (a1, a2) ->
      let v1 = eval a1 in
      let v2 = eval a2 in
      eval_top_reduce (App (v1, v2))
  | Let (x, a1, a2) ->
      let v1 = eval a1 in
      eval_top_reduce (Let (x, v1, a2))
  | (Fun _ | Const _ | Var _) as a -> eval_top_reduce a

(* Contexts as functions from terms to terms. *)
type context = expr -> expr

let hole : context = fun t -> t (* Empty context *)
let appL a t = App (t, a)
let appR a t = App (a, t)
let letL x a t = Let (x, t, a)

let ( ** ) (e1 : context) ((e0, a0) : context * expr) : context * expr =
  ((fun a -> e1 (e0 a)), a0)

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

(*
let rec eval_step = function
  | App (a1, a2) when not (evaluated a1) -> App (eval_step a1, a2)
  | App (a1, a2) when not (evaluated a2) -> App (a1, eval_step a2)
  | Let (x, a1, a2) when not (evaluated a1) -> Let (x, eval_step a1, a2)
  | a -> top_reduction a
*)

type context_z =
  | Top
  | AppL of context_z * expr
  | AppR of value * context_z
  | LetL of string * context_z * expr

and value = int * expr

let rec fill_context = function
  | Top, e -> e
  | AppL (c, l), e -> fill_context (c, App (e, l))
  | AppR ((_, e1), c), e2 -> fill_context (c, App (e1, e2))
  | LetL (x, c, e2), e1 -> fill_context (c, Let (x, e1, e2))

exception Error of context_z * expr
exception Value of int

let rec decompose_down ((c, e) : context_z * expr) : context_z * expr =
  match e with
  | Var _ -> raise (Error (c, e))
  | Const (Constr c) -> raise (Value (c.arity + 1))
  | Const (Prim c) -> raise (Value c.arity)
  | Fun (_, _) -> raise (Value 1)
  | Let (x, e1, e2) -> decompose_down (LetL (x, c, e2), e1)
  | App (e1, e2) -> (
      try decompose_down (AppL (c, e2), e1)
      with Value k1 -> (
        try decompose_down (AppR ((k1, e1), c), e2)
        with Value _ -> if k1 > 1 then raise (Value (k1 - 1)) else (c, e)))

let rec decompose_up k ((c, v) : context_z * expr) =
  if k > 0 then
    match c with
    | Top -> raise Not_found
    | LetL (x, c', e) -> (c', Let (x, v, e))
    | AppR ((k', v'), c') -> decompose_up (k' - 1) (c', App (v', v))
    | AppL (c', e) -> (
        try decompose_down (AppR ((k, v), c'), e)
        with Value _ -> decompose_up (k - 1) (c', App (v, e)))
  else (c, v)

let decompose ce = try decompose_down ce with Value k -> decompose_up k ce
let reduce_in ((c : context_z), e) = (c, top_reduction e)
let eval_step ce = reduce_in (decompose ce)
let rec eval_all ce = try eval_all (eval_step ce) with Not_found -> ce
let eval_z e = fill_context (eval_all (Top, e))

(* Printing *)
let hole = Const (Constr { name = Name "[]"; arity = 0 })

let rec expr_with expr_in_hole k out =
  let expr = expr_with expr_in_hole in
  let string x = Format.fprintf out x in
  let paren p f =
    if k > p then string "(";
    f ();
    if k > p then string ")"
  in
  function
  | Var x -> string "%s" x
  | Const _ as c when c = hole -> string "[%a]" (expr_with hole 0) expr_in_hole
  | Const (Constr { name = Int n; _ }) -> string "%d" n
  | Const (Constr { name = Name _; _ }) -> failwith "impossible"
  | Const (Prim { name = Name c; _ }) -> string "%s" c
  | Const (Prim { name = Int _; _ }) -> failwith "impossible"
  | Fun (x, a) -> paren 0 (fun () -> string "fun %s -> %a" x (expr 0) a)
  | App (App (Const (Prim { name = Name (("+" | "*") as n); _ }), a1), a2) ->
      paren 1 (fun () -> string "%a %s %a" (expr 2) a1 n (expr 2) a2)
  | App (a1, a2) -> paren 1 (fun () -> string "%a %a" (expr 1) a1 (expr 2) a2)
  | Let (x, a1, a2) ->
      paren 0 (fun () -> string "let %s = %a in %a" x (expr 0) a1 (expr 0) a2)

let print_context_expr (c, e) =
  expr_with e 0 Format.std_formatter (fill_context (c, hole))

let print_expr e = expr_with hole 0 Format.std_formatter e
let print_context c = print_expr (fill_context (c, hole))
let _ = print_expr
let _ = print_context
let _ = print_context_expr
