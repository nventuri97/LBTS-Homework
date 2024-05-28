open Env

(* Possible Language Expressions *)
type expr =
  | CstI of int
  | CstB of bool
  | CstString of string
  | Var of ide
  | Assign of ide * expr
  | Let of ide * expr * expr
  | Prim of ide * expr * expr
  | If of expr * expr * expr
  | Fun of ide * expr
  | Call of expr * expr
  | Abort of string
  | TrustBlock of trustContent
  | TrustedVar of ide
  | Include of expr
  | AccessTrust of expr * expr
  | Execute of expr
  | Assert of ide

(* TrustBlock only expressions *)
and trustContent =
  | LetSecret of ide * expr * trustContent
  | LetPublic of ide * expr * trustContent
  | Handle of ide * trustContent
  | EndTrustBlock

(* Runtime values *)
type value =
  | Int of int
  | Bool of bool
  | String of string
  | Closure of ide * expr * value env * value trustedList
  | ClosureInclude of expr * value env
  | Block of value trustedList * value env
