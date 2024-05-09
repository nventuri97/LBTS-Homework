open Ast
open Env
open Security

let rec evalTrustContent (tc : trustContent) (env : value env) (te : 'v trustedEnv)
(eval :
  expr ->
  value env->
  value) : value =
  match tc with
  | LetSecret (id, exprRight, next) ->
      let addsec = id :: (getSecret te) in
      let newTrustList = build (getTrust te) addsec (getHandle te)  in
      let id_value = eval exprRight env in
      let newEnv = extend env id id_value in
      evalTrustContent next newEnv newTrustList eval
  | LetPublic (id, exprRight, next) ->
    let addtrus = id :: (getTrust te) in
    let newTrustList = build addtrus (getSecret te) (getHandle te)  in
    let id_value = eval exprRight env in
    let newEnv = extend env id id_value in
      evalTrustContent next newEnv newTrustList eval
  | Handle (id, next) -> 
    if isIn id (getSecret te) then failwith "can't declare handle a secret"
      else if isIn id (getTrust te) then 
        let addhandle = id::(getHandle te) in
        let newTrustList = build (getTrust te) (getSecret te) addhandle in
        evalTrustContent next env newTrustList eval
      else failwith "can't add to handle list a variable not trusted"
  | EndTrustBlock -> Block(env)

let rec eval (e : expr) (env : value env): value =
  match e with
  | CstI i -> Int i
  | CstB b -> Bool (if b then true else false)
  | CstString s -> String s
    (* | Var (x, _) -> Value (lookup env x, taint_lookup env x) *)
  | Var (x) -> lookup env x
  | Assign(x, exprAssBody) ->
      let xVal = eval exprAssBody env  in
      let letenv = (x,xVal)::env in 
      eval exprAssBody letenv 
  | Let (x, exprRight, letBody) ->
      let xVal = eval exprRight env  in
      let letEnv = (x, xVal) :: env in
      eval letBody letEnv 
  | Prim (ope, e1, e2) -> (
      match (eval e1 env , eval e2 env ) with
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
      match eval e1 env  with
      | (Bool true) -> eval e2 env 
      | (Bool false) -> eval e3 env 
        (* | (_, _) -> failwith "If condition must be a boolean" *)
      | _ -> failwith "Improper use in If condition"
    )
  | Fun (x, fBody) -> (Closure (x, fBody, env))
    (*This part of the call to a function must be checked, here we have the error*)
  | Call (eFun, eArg) -> (
      let fClosure = eval eFun env  in
      match fClosure with
      | Closure (x, fBody, fDeclEnv) ->
            (* xVal is evaluated in the current  *)
          let xVal = eval eArg env  in
          let fBodyEnv = (x, xVal) :: fDeclEnv in
              (* fBody is evaluated in the updated  *)
          eval fBody fBodyEnv 
      | _ -> failwith "eval Call: not a function")
  | Abort msg -> failwith msg
  | GetInput(e) -> eval e env 
    (*Da ragionare ampiamente insieme*)
  | TrustBlock (tc) ->
    let newList = build [] [] [] in (*gli passo 3 liste come quelle che abbiamo usato in security*) 
      evalTrustContent tc env newList eval
  | Include (_, _, _) -> failwith "Not yet implemented"
  | Execute (_, _) -> failwith "Not yet implemented"
      (* match eval e1 env with
      | TrustBlock (_,_) -> failwith "Not yet implemented"
      | Include (_, _, _) -> failwith "Not yet implemented"
      | _ -> failwith "Impossible to execute" *)

let print_eval (ris : value) = (*Just to display on the terminal the evaluation result*)
      match ris with
      | Int(u) -> Printf.printf "evT = Int %d\n" u
      | Bool(u) -> Printf.printf "evT = Bool %b\n" u
      | String(u) -> Printf.printf "evT = Str %s\n" u
      | Block(_) -> Printf.printf "evT = TrustBlock created with SUCCESS!\n"
      | _ -> Printf.printf "Closure\n";;

      