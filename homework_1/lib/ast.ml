open Security
open Env


type expr =
  | CstI of int
  | CstB of bool
  | CstString of string
  | Var of ide * bool
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
  (*This part of the code is added in order to test the DTA*)
  | GetInput of expr    (*functions that takes input, taint source*)
  | TrustBlock of ide * trust_content
  | Include of ide * expr * expr
  | Execute of expr * expr
and trust_content =
  | LetSecret of ide * expr * trust_content
  | LetPublic of ide * expr * trust_content
  | Handle of ide * trust_content
  | EndTrustBlock

(*
  A runtime value is an integer or a function closure
  Boolean are encoded as integers.
*)
type value = 
  | Int of int
  | Bool of bool
  | String of string 
  | Value of value * bool
  | Closure of ide * expr * pdomain * value env * bool
(* In a closuer is saved also the pdomain *)

type value_with_taint = Value of value * bool

