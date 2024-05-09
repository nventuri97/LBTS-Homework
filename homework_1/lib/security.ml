open Utils

(*List that contains all the identificators definied in a trust block to understand
   if an indentificator is defined in a trust block or not*)
type trusted = ide list

(*Sublist of trusted that contains the identificators of secret variable or function*)
type secrets = ide list

(*Sublist of trusted that contains the indentificators of the function defined 
   as handle within a trustblock*)
type handleList = ide list

let rec isIn (x: 'v) (l: 'v list) : bool =
  match l with
  | [] -> false
  | head::tail -> if x=head then true else isIn x tail