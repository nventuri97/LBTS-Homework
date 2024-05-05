open Ast
open Env
open Security

let rec eval (e : expr) (env : value env) (t: bool) (stack : pstack): value =
    match e with
    | CstI i -> Int i
    | CstB b -> Bool (if b then true else false)
    | CstString s -> String s
    | Var (x, _) -> Value (lookup env x, taint_lookup env x)
    | Let (x, eRhs, letBody) ->
        let xVal = eval eRhs env t stack in
        let letEnv = (x, xVal, t) :: env in
        eval letBody letEnv t stack
    | SecLet (x, eRhs, secSet, letBody) ->
        (* xVal is evaluated in the current stack *)
        let xVal = eval eRhs env t stack in
        let letEnv = (x, xVal, t) :: env in
        let letStack = Grant secSet :: stack in
  
        (* letBody is evaluated in the updated stack *)
        eval letBody letEnv t letStack
    | Prim (ope, e1, e2) -> (
        match (eval e1 env t stack, eval e2 env t stack) with
        | (Value (Int i1, t1), Value (Int i2, t2)) -> (
            match ope with
            | "+" -> Value (Int (i1 + i2), t1 || t2)
            | "-" -> Value (Int (i1 - i2), t1 || t2)
            | "*" -> Value (Int (i1 * i2), t1 || t2)
            | "=" -> Value (Bool (i1 = i2), t1||  t2)
            | "<" -> Value (Bool (i1 < i2), t1  ||t2)
            | ">" -> Value (Bool (i1 > i2), t1  ||t2)
            | _ -> failwith "Unknown operator or wrong types for operation"
          )
        | _ -> failwith "Prim expects two integer arguments"
      ) 
    | If (e1, e2, e3) -> (
        match eval e1 env t stack with
        | Value (Bool true, t1) -> eval e2 env t1 stack
        | Value (Bool false, t1) -> eval e3 env t1 stack
        | Value (_, _) -> failwith "If condition must be a boolean"
        | _ -> failwith "Improper use in If condition"
      )
    | Fun (x, fBody, secSet) -> Value (Closure (x, fBody, secSet, env, t), t)
    (*This part of the call to a function must be checked, here we have the error*)
    | Call (eFun, eArg) -> (
        let fClosure = eval eFun env t stack in
        match fClosure with
        | Closure (x, fBody, secSet, fDeclEnv, t) ->
            (* xVal is evaluated in the current stack *)
            let xVal = eval eArg env t stack in
            let fBodyEnv = (x, xVal, t) :: fDeclEnv in
            let fBodyStack = Grant secSet :: stack in
            (* fBody is evaluated in the updated stack *)
            eval fBody fBodyEnv t fBodyStack
        | _ -> failwith "eval Call: not a function")
    (* The stuff below are useless for us*)
    | DemandPermission p -> Int (stackInspection inspect stack p)
    | OnPermission (p, e) ->
        if eval (DemandPermission p) env t stack = Int 1 then eval e env t stack
        else Int 0
    | CheckPermission p ->
        if eval (DemandPermission p) env t stack = Int 1 then Int 1
        else eval (Abort "CheckPermission failed") env t stack
    | Enable (p, e) -> eval e env t (Enable p :: stack)
    | Disable (p, e) -> eval e env t (Disable p :: stack)
    | SecBlock (sec, e) -> eval e env t (sec :: stack)
    | ReadFile f ->
        if
          eval (DemandPermission (Permission ("File", f, [ "r" ]))) env t stack
          = Int 1
        then Int 1 (* do read *)
        else eval (Abort ("No Read Permission for " ^ f)) env t stack
    | SendFile (e, f) ->
        if
          eval (DemandPermission (Permission ("File", f, [ "w" ]))) env t stack
          = Int 1
        then eval e env t stack (* do write *)
        else eval (Abort ("No Write Permission for " ^ f)) env t stack
    (*Useful for us*)
    | Abort msg -> failwith msg
    | GetInput(e) -> eval e env true stack
    (*Da ragionare ampiamente insieme*)
    | TrustBlock (blockName, content) ->
      (* Aggiungere la logica per gestire TrustBlock qui *)
      let trustBlockEnv = match content with
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
            eval (Handle (blockName, content)) trustBlockEnv t stack
    (*Da ragionare ampiamente insieme*)
    | Include (blockName, e1, e2) ->
      (* Aggiungi la logica per gestire Include qui *)
      let includedEnv = eval e1 env t stack in
      let includeContent = eval e2 includedEnv t stack in
      (* Chiamare ricorsivamente eval con il contenuto di Include nell'ambiente aggiornato *)
      eval includeContent includedEnv t stack
    (*Da ragionare ampiamente insieme*)
    | Execute (e1, e2) ->
      match eval e1 env t stack with
      | TrustBlock (_,_) -> failwith "Not yet implemented"
      | Include (_, _, _) -> failwith "Not yet implemented"
      | _ -> failwith "Impossible to execute"