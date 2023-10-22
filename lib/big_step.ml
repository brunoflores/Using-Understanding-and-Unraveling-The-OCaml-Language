open Syntax

(* Big-step operational semantics
   In small-step, values are always a subset of programs.
   In some cases, it is simpler to let values differ from programs. *)

type env = (string * value) list
and value = Closure of var * expr * env | Constant of constant * value list

type answer = Error | Value of value

let val_int u = Value (Constant (Constr { name = Int u; arity = 0 }, []))

let delta c l =
  match (c, l) with
  | ( Prim { name = Name "+"; _ },
      [
        Constant (Constr { name = Int u; _ }, []);
        Constant (Constr { name = Int v; _ }, []);
      ] ) ->
      val_int (u + v)
  | ( Prim { name = Name "*"; _ },
      [
        Constant (Constr { name = Int u; _ }, []);
        Constant (Constr { name = Int v; _ }, []);
      ] ) ->
      val_int (u * v)
  | _ -> Error

let get x env = try Value (List.assoc x env) with Not_found -> Error

let rec eval env = function
  | Var x -> get x env
  | Const c -> Value (Constant (c, []))
  | Fun (x, a) -> Value (Closure (x, a, env))
  | Let (x, a1, a2) -> (
      match eval env a1 with
      | Value v1 -> eval ((x, v1) :: env) a2
      | Error -> Error)
  | App (a1, a2) -> (
      match eval env a1 with
      | Value v1 -> (
          match (v1, eval env a2) with
          | Constant (c, l), Value v2 -> (
              let k = List.length l + 1 in
              let arity =
                match c with
                | Constr { arity; _ } -> arity
                | Prim { arity; _ } -> arity
              in
              if arity < k then Error
              else if arity > k then Value (Constant (c, v2 :: l))
              else
                match c with
                | Constr _ -> Value (Constant (c, v2 :: l))
                | _ -> delta c (v2 :: l))
          | Closure (x, e, env0), Value v2 -> eval ((x, v2) :: env0) e
          | _, Error -> Error)
      | Error -> Error)

let eval_big_step e =
  match eval [] e with
  | Value (Constant ((Constr { name = Int _; _ } as c), _)) -> Const c
  | _ -> failwith ""

(*
  exception Undefined_constant of string

  let type_of_const c =
    let int3 = tarrow tint (tarrow tint tint) in
    match c.name with
    | Int _ -> tint
    | Name ("+" | "*") -> int3
    | Name n -> raise (Undefined_constant n)
  *)
