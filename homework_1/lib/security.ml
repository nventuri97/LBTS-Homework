open Utils

(*
    A permission is a triple Set, Entity, List of Allowed Actions
    where a Set is the set of Entites (e.g. File, Network...)
    Entity is the identifier of an element of the set (if '*' is used then the permission is for all the elements in the set)
    and Allowed Actions is the list of allowed actions for that entity
*)
type permission = Permission of string * string * string list

(*
    A permission domain is a list of permissions
*)
type pdomain = permission list

(**
    A secAction is a security action (e.g grant of a pdomain or enable/disable a permission)
*)
type secAction =
  | Grant of pdomain
  | Disable of permission
  | Enable of permission

(*
    A pstack is the runtime stack
*)
type pstack = secAction list

(*
    Policy Manager Definable
    return true iff the permission request is allowed by the permission p
    Can be redefined according to different policies

    Here we:
      - match the type of resource 
      - match the filename or any filename
      - check associated permissions (r and/or w)
*)
let allows (p : permission) (request : permission) =
  let (Permission (s1, r1, a1)) = p in
  let (Permission (s2, r2, a2)) = request in
  s1 = s2 && (r1 = "*" || r1 = r2) && sublist a2 a1

(*
  Tests whether a pdomain allows a given permission
*)
let rec domainInspection (set : pdomain) (request : permission) =
  match set with
  | [] -> 0
  | p :: s -> if allows p request then 1 else domainInspection s request

(*
  Inspect the stack: if empty return false otherwise calls an inspectFunction that applies a given inspection policy
*)
let stackInspection inspectFunction (stack : pstack) (request : permission) =
  match stack with [] -> 0 | e :: l -> inspectFunction (e :: l) request

(*
  Policy Manager Definable
  Inspect recursively the stack: 
    if empty return true otherwise:
      - if the first element is a domain check if the domain allows the permission (if true continues the inspection otherwise return false)
      - else if the first element is an enable return true if the enable allows the request otherwise calls stackInspection
      - else if the first element is an disable return false if the disable allows the request otherwise calls stackInspection
      
  Enable and Disable must call stackInspection instead of inspect 
  because in this case if the stack is empty you must return false instead of true. 
  
  If, on the other hand, there is a pdomain, it is called inspect 
  because the presence of the empty list indicates the end of the inspection and therefore true.

  This function can also be redefined to implement different inspection policies
*)
let rec inspect (stack : pstack) (request : permission) =
  match stack with
  | [] -> 1
  | sec :: s -> (
      match sec with
      | Grant domain ->
          if domainInspection domain request = 1 then inspect s request else 0
      | Enable p ->
          if allows p request then 1 else stackInspection inspect s request
      | Disable p ->
          if allows p request then 0 else stackInspection inspect s request)
