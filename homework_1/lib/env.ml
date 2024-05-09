(*
  Variable identifiers are strings
*)
type ide = string

(*
  An environment is a map from identifier to a value (what the identifier is bound to).
  For simplicity we represent the environment as an association list, i.e., a list of pair (identifier, data).
*)
(* type 'v env = (ide * 'v * bool) list *)
type 'v env = (ide * 'v) list

(*For extend an environment*)
let extend (e : 'v env) (id : ide) (v : 'v) : 'v env = (id, v) :: e
(*
  Given an environment {env} and an identifier {x} it returns the data {x} is bound to.
  If there is no binding, it raises an exception.
*)
let rec lookup env x =
  match env with
  | [] -> failwith (x ^ " not found")
  | (y, v) :: r -> if x = y then v else lookup r x

let rec taint_lookup env x=
  match env with
  | [] -> failwith (x ^ " not found")
  | (y, _, t)::r -> if x = y then t else taint_lookup r x 