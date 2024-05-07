open Ast
open Env

let rec eval (e : expr) (env : value env) (stack : stack): value =
    match e with
    | CstI i -> Int i
    | CstB b -> Bool (if b then true else false)
    | CstString s -> String s
    (* | Var (x, _) -> Value (lookup env x, taint_lookup env x) *)
    | Var (x) -> lookup env x
    | Assign(x, exprAssBody) ->
        let xVal = eval exprAssBody env stack in
        let letenv = (x,xVal)::env in 
          eval exprAssBody letenv stack
    | Let (x, exprRight, letBody) ->
        let xVal = eval exprRight env stack in
        let letEnv = (x, xVal) :: env in
            eval letBody letEnv stack
    | Prim (ope, e1, e2) -> (
        match (eval e1 env stack, eval e2 env stack) with
        | ((Int i1), (Int i2)) -> (
            match ope with
            | "+" -> (Int (i1 + i2))
            | "-" -> (Int (i1 - i2))
            | "*" -> (Int (i1 * i2))
            | "=" -> (Bool (i1 = i2))
            | "<" -> (Bool (i1 < i2))
            | ">" -> (Bool (i1 > i2))
            | _ -> failwith "Unknown operator or wrong types for operation"
          )
        | ((Bool b1), (Bool b2))->(
          match ope with
            | "||"-> (Bool(b1||b2))
            | "&&"-> (Bool(b1&&b2))
            | _ -> failwith "Unknown operator or wrong types for operation"
        )
        | _ -> failwith "Prim expects two integer arguments"
      ) 
    | If (e1, e2, e3) -> (
        match eval e1 env stack with
        | (Bool true) -> eval e2 env stack
        | (Bool false) -> eval e3 env stack
        (* | (_, _) -> failwith "If condition must be a boolean" *)
        | _ -> failwith "Improper use in If condition"
      )
    | Fun (x, fBody) -> (Closure (x, fBody, env))
    (*This part of the call to a function must be checked, here we have the error*)
    | Call (eFun, eArg) -> (
        let fClosure = eval eFun env stack in
        match fClosure with
        | Closure (x, fBody, fDeclEnv) ->
            (* xVal is evaluated in the current stack *)
            let xVal = eval eArg env stack in
            let fBodyEnv = (x, xVal) :: fDeclEnv in
              (* fBody is evaluated in the updated stack *)
              eval fBody fBodyEnv stack
        | _ -> failwith "eval Call: not a function")
    | Abort msg -> failwith msg
    | GetInput(e) -> eval e env stack
    (*Da ragionare ampiamente insieme*)
    | TrustBlock trustC -> 
      match trustC with
      | LetSecret (idLS, exp, body) -> 
        let addsec = idLS :: (getSecret env) in  (*getsecret e gethandle prendono un env e ritornano la lista di ide secret e ide handle*)
          let xVal = eval exp env stack in 
            let letenv = (idLS,xVal)::env in 
              eval body letenv stack
      | LetPublic -> failwith "error"
      | Handle -> failwith "error"
      | EndTrustBlock -> failwith "error"
      | _ -> failwith "error"
      
      (* | _ failwith "Not yet implemented"
        Aggiungere la logica per gestire TrustBlock qui *)
      (* let trustBlockEnv = match content with
        | LetSecret (x, e, tc) ->
            let xVal = eval e env t stack in
            (x, xVal, t) :: env
        | LetPublic (x, e, tc) ->
            let xVal = eval e env t stack in
            (x, xVal, t) :: env
        | Handle (x, tc) ->
            (* Aggiungi la logica per gestire Handle qui *)
            env
        | EndTrustBlock -> env in
          (* Chiamare ricorsivamente eval con il contenuto di TrustBlock nell'ambiente aggiornato *)
            eval (Handle (blockName, content)) trustBlockEnv t stack *)
    (*Da ragionare ampiamente insieme*)
    | Include (_, _, _) -> failwith "Not yet implemented"
      (* Aggiungi la logica per gestire Include qui *)
      (* let includedEnv = eval e1 env t stack in
      let includeContent = eval e2 includedEnv t stack in
      (* Chiamare ricorsivamente eval con il contenuto di Include nell'ambiente aggiornato *)
      eval includeContent includedEnv t stack *)
    (*Da ragionare ampiamente insieme*)
    | Execute (_, _) -> failwith "Not yet implemented"
      (* match eval e1 env t stack with
      | TrustBlock (_,_) -> failwith "Not yet implemented"
      | Include (_, _, _) -> failwith "Not yet implemented"
      | _ -> failwith "Impossible to execute" *)

let print_eval (ris : value) = (*Just to display on the terminal the evaluation result*)
	match ris with
		| Int(u) -> Printf.printf "evT = Int %d\n" u
		| Bool(u) -> Printf.printf "evT = Bool %b\n" u
		| String(u) -> Printf.printf "evT = Str %s\n" u
		| _ -> Printf.printf "Closure\n";;