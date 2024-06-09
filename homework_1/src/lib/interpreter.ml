open Ast
open Env

let rec evalTrustContent (tc : trustContent) (env : value env)
    (te : value secureTuple)
    (eval : expr -> value env -> bool -> value secureTuple -> value * bool) :
    value =
  match tc with
  | LetSecret (id, exprRight, next) ->(
    match exprRight with
    | TrustBlock (_) -> failwith "can't create a trustblock inside a trustblock"
    | _ ->
      let addsec = id :: getSecret te in
      let addTrust = id :: getTrust te in
      let newSeclist = build addTrust addsec (getHandle te) in
      let id_value, taintness = eval exprRight env false newSeclist in
      let newEnv = extend env id id_value taintness in
      evalTrustContent next newEnv newSeclist eval)
  | LetPublic (id, exprRight, next) ->(
    match exprRight with
    | TrustBlock (_) -> failwith "can't create a trustblock inside a trustblock"
    | _ ->(
      let addtrus = id :: getTrust te in
      let newTrustList = build addtrus (getSecret te) (getHandle te) in
      let id_value, taintness = eval exprRight env false newTrustList in
      let newEnv = extend env id id_value taintness in
      evalTrustContent next newEnv newTrustList eval))
  | Handle (id, next) ->
      if isIn id (getSecret te) then failwith "can't declare handle a secret"
      else if isIn id (getTrust te) then
        let addhandle = id :: getHandle te in
        let newTrustList = build (getTrust te) (getSecret te) addhandle in
        evalTrustContent next env newTrustList eval
      else failwith "can't add to handle list a variable not trusted"
  | EndTrustBlock -> Block (te, env)

let env = []
let list = ([], [], [])

let rec eval (e : expr) (env : value env) (t : bool) (te : value secureTuple) :
    value * bool =
  match e with
  | CstI i -> (Int i, t)
  | CstB b -> (Bool b, t)
  | CstString s -> (String s, t)
  | Var x ->
      if
        isIn x (getTrust te)
        && (isIn x (getSecret te) || not (isIn x (getHandle te)))
      then failwith ("Cannot access var " ^ x ^ " without permission")
      else (lookup env x, taint_lookup env x)
  | Assign (x, exprAssBody) ->
      let xVal, taintness = eval exprAssBody env t te in
      let letenv = extend env x xVal taintness in
      eval exprAssBody letenv taintness te
  | Let (x, exprRight, letBody) ->
      let xVal, taintness = eval exprRight env t te in
      let letEnv = extend env x xVal taintness in
      eval letBody letEnv t te
  | Prim (ope, e1, e2) -> (
      let v1, t1 = eval e1 env t te in
      let v2, t2 = eval e2 env t te in
      match ((v1, t1), (v2, t2)) with
      | (Int i1, _), (Int i2, _) -> (
          match ope with
          | "+" -> (Int (i1 + i2), t1 || t2)
          | "-" -> (Int (i1 - i2), t1 || t2)
          | "*" -> (Int (i1 * i2), t1 || t2)
          | "=" -> (Bool (i1 = i2), t1 || t2)
          | "<" -> (Bool (i1 < i2), t1 || t2)
          | ">" -> (Bool (i1 > i2), t1 || t2)
          | _ -> failwith "Unknown operator or wrong types for operation")
      | (Bool b1, _), (Bool b2, _) -> (
          match ope with
          | "||" -> (Bool (b1 || b2), t1 || t2)
          | "&&" -> (Bool (b1 && b2), t1 || t2)
          | _ -> failwith "Unknown operator or wrong types for operation")
      | ((String s1, _), (String s2, _)) -> (
        match ope with
        | "=" -> (Bool (s1 = s2),t1 || t2)
        | _ -> failwith "Unknown operator or wrong types for operation"
      )
      | _ -> failwith "Prim expects two integer arguments")
  | If (e1, e2, e3) -> (
      let v1, t1 = eval e1 env t te in
      match (v1, t1) with
      | Bool true, _ ->
          let v2, t2 = eval e2 env t te in
          (v2, t1 || t2)
      | Bool false, _ ->
          let v3, t3 = eval e3 env t te in
          (v3, t1 || t3)
      | _ -> failwith "Improper use in If condition")
  | Fun (x, fBody) ->
      (Closure (x, fBody, env, te), t)
  | Call (eFun, eArg) -> (
      let fClosure, tClosure = eval eFun env t te in
      match fClosure with
      | Closure (x, fBody, fDeclEnv, te) ->
          (* xVal is evaluated in the current env *)
          let xVal, taintness = eval eArg env tClosure te in
            let fBodyEnv = (x, xVal, taintness) :: fDeclEnv in
              (* fBody is evaluated in the updated env *)
              eval fBody fBodyEnv taintness te
      | _ -> failwith "eval Call: not a function")
  | Abort msg -> failwith msg
  | TrustBlock tc ->
      if t then failwith "Tainted sources cannot access trust block"
      else
        let trustBlockEval = evalTrustContent tc env te eval in
        (trustBlockEval, false)
  | TrustedVar x ->
      if isIn x (getTrust te) then (lookup env x, taint_lookup env x)
      else failwith ("The ide " ^ x ^ " doesn't exist or it belongs to another env")
  | Include iBody -> (
      match iBody with
      | Include _ -> failwith "Cannot nest include blocks"
      | TrustBlock _ ->
          failwith "Cannot create TrustBlocks inside an Include"
      | _ -> (ClosureInclude (iBody, env), t))
  | Execute extCode -> (
      let fClosure, _ = eval extCode env t te in
      match fClosure with
      | ClosureInclude (fBody, fDeclEnv) -> eval fBody fDeclEnv true te
      | _ -> failwith "eval Call: not a function")
  | AccessTrust (ideTrust, ideVar) -> (
      if t then failwith "Tainted sources cannot access trust block"
      else
        let trustV, taintV = eval ideTrust env t te in
        match (trustV, taintV) with
        | Block (list, secondEnv), taintV -> eval ideVar secondEnv taintV list
        | _ -> failwith "the access must be applied to an trustblock")
  | Assert ide ->
      let taintness = taint_lookup env ide in
      if taintness then
        failwith ("Assertion Failed - Var \"" ^ ide ^ "\" is tainted")
      else (lookup env ide, taintness)

let print_ide_list ide_list =
  List.iter
    (fun ide ->
      print_string ide;
      print_string " ")
    ide_list;
  print_newline ()

let print_trustedEnv (env : 'v secureTuple) =
  let t, s, h = env in
  print_string "Trusted: ";
  print_ide_list t;
  print_string "Secret: ";
  print_ide_list s;
  print_string "Handled: ";
  print_ide_list h;
  print_newline ()


 let feval (t: expr)= eval t env false list

let print_eval (ris : value * bool) =
  (*Just to display on the terminal the evaluation result*)
  match ris with
  | Int u, t -> Printf.printf " Result: Int %d, Taintness: %b\n" u t
  | Bool u, t -> Printf.printf " Result: Bool %b, Taintness: %b \n" u t
  | String u, t -> Printf.printf " Result: String %s, Taintness: %b\n" u t
  | Block (_, _), _ -> Printf.printf " Result: Block created succesfully\n"
  | _ -> Printf.printf " Closure\n"
