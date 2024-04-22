open Security
open Env

type expr =
  | CstI of int
  | CstB of bool
  | Var of ide
  | Let of ide * expr * expr
  (* SecLet evaluates the expressions pushing the given pdomain on top of the stack *)
  | SecLet of ide * expr * pdomain * expr
  | Prim of ide * expr * expr
  | If of expr * expr * expr
  (* Lambda: parameters, body and permission domain *)
  | Fun of ide * expr * pdomain
  | Call of expr * expr
  (* Return true iff that permission is allowed *)
  | DemandPermission of permission
  (* Evaluates the expression iff DemandPermission is true otherwise return false (Int 0) *)
  | OnPermission of permission * expr
  (* Aborts if permission is not enabled *)
  | CheckPermission of permission
  (* Evaluates the expression with the permission enabled *)
  | Enable of permission * expr
  (* Evaluates the expression with the permission disabled *)
  | Disable of permission * expr
  (* Evaluates the expression pushing the secAction on top of the stack *)
  | SecBlock of secAction * expr
  (* Reads a file iff is allowed otherwise aborts *)
  | ReadFile of string
    (* Send and evaluates expr to a file iff is allowed otherwise aborts *)
  | SendFile of expr * string
  | Abort of string

(*
  A runtime value is an integer or a function closure
  Boolean are encoded as integers.
*)
type value = Int of int | Closure of ide * expr * pdomain * value env
(* In a closuer is saved also the pdomain *)
