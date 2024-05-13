open Ast
open Env

let rec evalTrustContent (tc : trustContent) (env : value env) (te : value trustedList)
    (eval :
       expr ->
     value env->
     value trustedList ->
     value) : value =
  match tc with
  | LetSecret (id, exprRight, next) ->
    let addsec = id :: (getSecret te) in
    let addTrust = id :: (getTrust te) in
    let newSeclist = build addTrust addsec (getHandle te) in
    let id_value = eval exprRight env newSeclist in
    let newEnv = extend env id id_value in
    evalTrustContent next newEnv newSeclist eval
  | LetPublic (id, exprRight, next) ->
      let addtrus = id :: (getTrust te) in
      let newTrustList = build addtrus (getSecret te) (getHandle te)  in
      let id_value = eval exprRight env newTrustList in
      let newEnv = extend env id id_value in
      evalTrustContent next newEnv newTrustList eval
  | Handle (id, next) ->  (*aggiungere il caso in cui quello che chiama la id non utilizzi cose trusted*)
      if isIn id (getSecret te) then failwith "can't declare handle a secret"
      else if isIn id (getTrust te) then 
        let addhandle = id::(getHandle te) in
        let newTrustList = build (getTrust te) (getSecret te) addhandle in
        evalTrustContent next env newTrustList eval
      else failwith "can't add to handle list a variable not trusted"
  | EndTrustBlock -> Block("Trustblock created with success")
                       

let rec eval (e : expr) (env : value env) (te : value trustedList): value =
  match e with
  | CstI i -> Int i
  | CstB b -> Bool (if b then true else false)
  | CstString s -> String s
    (* | Var (x, _) -> Value (lookup env x, taint_lookup env x) *)
  | Var (x) -> 
    if ( isIn x (getTrust te) && (isIn x (getSecret te)) && not (isIn x (getHandle te))) then 
      failwith "You are trying to access to a var without permission"
    else 
      lookup env x 
  | Assign(x, exprAssBody) ->
      let xVal = eval exprAssBody env te  in
      let letenv = (x,xVal)::env in 
      eval exprAssBody letenv te
  | Let (x, exprRight, letBody) ->
      let xVal = eval exprRight env te in
      let letEnv = (x, xVal) :: env in
      eval letBody letEnv te
  | Prim (ope, e1, e2) -> (
      match (eval e1 env te, eval e2 env te ) with
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
      | ((Bool b1), (Bool b2))-> (
          match ope with
          | "||" -> (Bool (b1 || b2))
          | "&&" -> (Bool (b1 && b2))
          | _ -> failwith "Unknown operator or wrong types for operation"
        )
      | _ -> failwith "Prim expects two integer arguments"
    ) 
  | If (e1, e2, e3) -> (
      match eval e1 env te with
      | (Bool true) -> eval e2 env te
      | (Bool false) -> eval e3 env te
        (* | (_, _) -> failwith "If condition must be a boolean" *)
      | _ -> failwith "Improper use in If condition"
    )
  | Fun (x, fBody) -> (Closure (x, fBody, env))
    (*This part of the call to a function must be checked, here we have the error*)
  | Call (eFun, eArg) -> (
      let fClosure = eval eFun env te in
      match fClosure with
      | Closure (x, fBody, fDeclEnv) ->
            (* xVal is evaluated in the current  *)
          let xVal = eval eArg env te in
          let fBodyEnv = (x, xVal) :: fDeclEnv in
              (* fBody is evaluated in the updated  *)
          eval fBody fBodyEnv te
      | _ -> failwith "eval Call: not a function")
  | Abort msg -> failwith msg
  | GetInput(e) -> eval e env te
    (*Da ragionare ampiamente insieme*)
  | TrustBlock (tc) ->
      (*let newList = build [] [] [] in gli passo 3 liste come quelle che abbiamo usato in security*) 
      evalTrustContent tc env te eval
  | Include (iBody) -> 
    (match iBody with
       | Include(_) -> failwith "you cant include inside an include"
       | TrustBlock(_)-> failwith "you cant create A trustBlock inside an include"
       | _ -> (ClosureInclude (iBody, env)))
  | Execute (extCode) -> (
      let fClosure = eval extCode env te in
      match fClosure with
      | ClosureInclude (fBody, fDeclEnv) ->
          eval fBody fDeclEnv te
      | _ -> failwith "eval Call: not a function")


let print_ide_list ide_list = 
  List.iter (fun ide -> print_string ide; print_string " ") ide_list;
  print_newline ()
    
let print_trustedEnv (env : 'v trustedList) =
  let (t, s, h) = env in 
  print_string "Trusted: ";
  print_ide_list t;
  print_string "Secret: ";
  print_ide_list s;
  print_string "HandleList: ";
  print_ide_list h;
  print_newline ()

      
let print_eval (ris : value) = (*Just to display on the terminal the evaluation result*)
  match ris with
  | Int(u) -> Printf.printf "evT = Int %d\n" u
  | Bool(u) -> Printf.printf "evT = Bool %b\n" u
  | String(u) -> Printf.printf "evT = Str %s\n" u
  | Block(u) -> Printf.printf "evT = %s\n" u
  | _ -> Printf.printf "Closure\n";;
