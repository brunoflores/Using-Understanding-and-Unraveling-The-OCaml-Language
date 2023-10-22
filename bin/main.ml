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

(*
utop[8]> eval_z example;;
decompose <-- [(fun x -> (x + 1) * (x + -1)) ((fun x -> x + 1) 2)]
decompose --> (fun x -> (x + 1) * (x + -1)) [(fun x -> x + 1) 2]
reduce_in <-- (fun x -> (x + 1) * (x + -1)) [(fun x -> x + 1) 2]
reduce_in --> (fun x -> (x + 1) * (x + -1)) [2 + 1]
decompose <-- (fun x -> (x + 1) * (x + -1)) [2 + 1]
decompose --> (fun x -> (x + 1) * (x + -1)) [2 + 1]
reduce_in <-- (fun x -> (x + 1) * (x + -1)) [2 + 1]
reduce_in --> (fun x -> (x + 1) * (x + -1)) [3]
decompose <-- (fun x -> (x + 1) * (x + -1)) [3]
decompose --> [(fun x -> (x + 1) * (x + -1)) 3]
reduce_in <-- [(fun x -> (x + 1) * (x + -1)) 3]
reduce_in --> [(3 + 1) * (3 + -1)]
decompose <-- [(3 + 1) * (3 + -1)]
decompose --> [3 + 1] * (3 + -1)
reduce_in <-- [3 + 1] * (3 + -1)
reduce_in --> [4] * (3 + -1)
decompose <-- [4] * (3 + -1)
decompose --> 4 * [3 + -1]
reduce_in <-- 4 * [3 + -1]
reduce_in --> 4 * [2]
decompose <-- 4 * [2]
decompose --> [4 * 2]
reduce_in <-- [4 * 2]
reduce_in --> [8]
decompose <-- [8]
decompose raises Not_found
- : expr = Const (Constr {Unraveling_ocaml.Syntax.name = Int 8; arity = 0})
*)

let _ =
  let print a =
    match a with
    | Const (Constr { name = Int n; _ }) -> n
    | _ -> failwith "not a number"
  in
  let e = eval example in
  let _ = Printf.printf "%d\n" @@ print e in
  let e' = eval_steps example in
  let _ = Printf.printf "%d\n" @@ print e' in
  let e'' = eval_z example in
  let _ = Printf.printf "%d\n" @@ print e'' in
  ()

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
