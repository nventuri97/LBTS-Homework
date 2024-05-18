
type expr =
  | CstI of int
  | CstB of bool
  | CstString of string
  (* | Var of ide * bool *)
  | Var of ide
  | Assign of ide * expr (*let x=...*)
  | Let of ide * expr * expr (*let x= x in...*)
  | Prim of ide * expr * expr
  | If of expr * expr * expr
  (* Lambda: parameters, body and permission domain *)
  | Fun of ide * expr 
  | Call of expr * expr
  | Abort of string
  (*This part of the code is added in order to test the DTA*)
  | TrustBlock of trustContent
  | TrustedVar of ide
  | Include of expr
  | AccessTrust of expr * expr
  | Execute of expr
  | Assert of ide
and trustContent =
  | LetSecret of ide * expr * trustContent
  | LetPublic of ide * expr * trustContent
  | Handle of ide * trustContent
  | EndTrustBlock 

(*
A runtime value is an integer or a function closure
    Boolean are encoded as integers.
                         *)
type value = 
  | Int of int
  | Bool of bool
  | String of string 
  (* | Value of value * bool *)
  (* | Closure of ide * expr * value env * bool *)
  | Closure of ide * expr * value env
  | ClosureInclude of expr * value env
  | Block of value trustedList * value env
