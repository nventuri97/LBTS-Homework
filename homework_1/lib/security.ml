open Env

(*List that contains all the identificators definied in a trust block to understand
   if an indentificator is defined in a trust block or not*)
type trusted = ide list

   (*Sublist of trusted that contains the identificators of secret variable or function*)
type secret = ide list
   
   (*Sublist of trusted that contains the indentificators of the function defined 
      as handle within a trustblock*)
type handleList = ide list

type 'v trustedList = trusted * secret * handleList
   
let rec isIn (x: 'v) (l: 'v list) : bool =
  match l with
  | [] -> false
  | head::tail -> if x=head then true else isIn x tail
             
let build (t: trusted) (s: secret) (h: handleList) : 'v trustedList = (t, s, h)

let getTrust (e : 'v trustedList) : trusted =
  let trustedL, _, _ = e in
  trustedL
  
  (*
    Get the gateways from a trusted environment.
  *)
let getHandle (e :  'v trustedList) : handleList =
  let _, handle, _ = e in
  handle
  
  (*
    Get the secrets from a trusted environment.
  *)
let getSecret (e : 'v trustedList) : secret =
  let _, _, secr = e in
  secr
