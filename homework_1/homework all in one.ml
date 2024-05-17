(*------------------------------------------------------------------------*)
(*----------------------------------ENV-----------------------------------*)
(*------------------------------------------------------------------------*)
(*
  Variable identifiers are strings
*)
type ide = string

(*
  An environment is a map from identifier to a value (what the identifier is bound to).
  For simplicity we represent the environment as an association list, i.e., a list of pair (identifier, data).
*)
(* type 'v env = (ide * 'v * bool) list *)
type 'v env = (ide * 'v * bool) list

(*To extend an environment*)
let extend (e : 'v env) (id : ide) (v : 'v) (t: bool) : 'v env = (id, v, t) :: e
(*
  Given an environment {env} and an identifier {x} it returns the data {x} is bound to.
  If there is no binding, it raises an exception.
*)
let rec lookup env x =
  match env with
  | [] -> failwith (x ^ " not found")
  | (y, v, _) :: r -> if x = y then v else lookup r x

let rec taint_lookup env x=
  match env with
  | [] -> failwith (x ^ " not found")
  | (y, _, t)::r -> if x = y then t else taint_lookup r x 


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
  let _, _, handle = e in
  handle
 
 (*
   Get the secrets from a trusted environment.
 *)
let getSecret (e : 'v trustedList) : secret =
  let _, secr, _ = e in
  secr  
          
(*------------------------------------------------------------------------*)
(*------------------------------SECURITY----------------------------------*)
(*------------------------------------------------------------------------*)

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
    Get the secrets from a trusted environment.
  *)
let getSecret (e : 'v trustedList) : secret =
  let _, secr, _ = e in
  secr
    
  (*
    Get the gateways from a trusted environment.
  *)
let getHandle (e :  'v trustedList) : handleList =
  let _, _, handle = e in
  handle



(*------------------------------------------------------------------------*)
(*----------------------------------AST-----------------------------------*)
(*------------------------------------------------------------------------*) 

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
               
(*------------------------------------------------------------------------*)
(*------------------------------INTERPRETER-------------------------------*)
(*------------------------------------------------------------------------*)

let rec evalTrustContent (tc : trustContent) (env : value env) (te : value trustedList)
    (eval :
       expr ->
     value env->
     bool ->
     value trustedList ->
      (*Qui molto probabilmente deve ritornare un value * bool come la eval normale*)
     value * bool) : value =
  match tc with
  | LetSecret (id, exprRight, next) ->
      let addsec = id :: (getSecret te) in
      let addTrust = id :: (getTrust te) in
      let newSeclist = build addTrust addsec (getHandle te) in
      let (id_value, taintness) = eval exprRight env false newSeclist in
      let newEnv = extend env id id_value taintness in
      evalTrustContent next newEnv newSeclist eval
  | LetPublic (id, exprRight, next) ->
      let addtrus = id :: (getTrust te) in
      let newTrustList = build addtrus (getSecret te) (getHandle te)  in
      let (id_value, taintness) = eval exprRight env false newTrustList in
      let newEnv = extend env id id_value taintness in
      evalTrustContent next newEnv newTrustList eval
  | Handle (id, next) ->  (*aggiungere il caso in cui quello che chiama la id non utilizzi cose trusted*)
      if isIn id (getSecret te) then failwith "can't declare handle a secret"
      else if isIn id (getTrust te) then 
        let addhandle = id::(getHandle te) in
        let newTrustList = build (getTrust te) (getSecret te) addhandle in
        evalTrustContent next env newTrustList eval
      else failwith "can't add to handle list a variable not trusted"
  | EndTrustBlock -> Block(te,env)
                       

let rec eval (e : expr) (env : value env) (t: bool) (te : value trustedList): (value * bool)=
  match e with
  | CstI i -> (Int i, t)
  | CstB b -> (Bool b, t)
  | CstString s -> (String s, t)
  | Var (x) ->
      if ( isIn x (getTrust te) && 
           (isIn x (getSecret te) || not (isIn x (getHandle te)))
         ) then 
        failwith ( "Cannot access var "^ x ^" without permission")
      else 
        (lookup env x, taint_lookup env x)
  | Assign(x, exprAssBody) ->
      let (xVal, taintness) = eval exprAssBody env t te  in
      let letenv = extend env x xVal taintness in 
      eval exprAssBody letenv taintness te
  | Let (x, exprRight, letBody) ->
      let (xVal, taintness) = eval exprRight env t te in
      let letEnv = extend env x xVal taintness in
      eval letBody letEnv t te
  | Prim (ope, e1, e2) -> (
      let (v1, t1) = eval e1 env t te in
      let (v2, t2) = eval e2 env t te in
      match ((v1,t1), (v2, t2) ) with
      | ((Int i1, _), (Int i2, _)) -> (
          match ope with
          | "+" -> (Int (i1 + i2),t1 || t2)
          | "-" -> (Int (i1 - i2),t1 || t2)
          | "*" -> (Int (i1 * i2),t1 || t2)
          | "=" -> (Bool (i1 = i2),t1 || t2)
          | "<" -> (Bool (i1 < i2),t1 || t2)
          | ">" -> (Bool (i1 > i2),t1 || t2)
          | _ -> failwith "Unknown operator or wrong types for operation"
        )
      | ((Bool b1, _), (Bool b2, _))-> (
          match ope with
          | "||" -> (Bool (b1 || b2),t1 || t2)
          | "&&" -> (Bool (b1 && b2),t1 || t2)
          | _ -> failwith "Unknown operator or wrong types for operation"
        )
      | _ -> failwith "Prim expects two integer arguments"
    ) 
  | If (e1, e2, e3) -> (
      let (v1,t1)= eval e1 env t te in
      match (v1, t1) with
      | (Bool true, _) -> let (v2,t2) = eval e2 env t te in (v2, t1 || t2)
      | (Bool false, _) -> let (v3, t3) = eval e3 env t te in (v3, t1 || t3)
      | _ -> failwith "Improper use in If condition"
    )
  | Fun (x, fBody) -> (Closure (x, fBody, env), t)
    (*This part of the call to a function must be checked, here we have the error*)
  | Call (eFun, eArg) -> (
      let (fClosure, tClosure) = eval eFun env t te in
      match fClosure with
      | Closure (x, fBody, fDeclEnv) ->
            (* xVal is evaluated in the current  *)
          let (xVal, taintness) = eval eArg env tClosure te in
          let fBodyEnv = (x, xVal, taintness) :: fDeclEnv in
              (* fBody is evaluated in the updated  *)
          eval fBody fBodyEnv taintness te
      | _ -> failwith "eval Call: not a function")
  | Abort msg -> failwith msg 
  | TrustBlock (tc) ->
      if t then
        failwith "Tainted sources cannot access trust block"
      else (
        let trustBlockEval = evalTrustContent tc env te eval in
        (trustBlockEval, false)
      )
  | TrustedVar x ->
      if isIn x (getTrust te) 
      then (lookup env x, taint_lookup env x) 
      else failwith "This ide doesn't exist or it belongs to another env"
  | Include (iBody) -> 
      (match iBody with
       | Include(_) -> failwith "you cant include inside an include"
       | TrustBlock(_)-> failwith "you cant create A trustBlock inside an include"
       | _ -> (ClosureInclude (iBody, env), t))
  | Execute (extCode) -> (
      let (fClosure, taintV) = eval extCode env t te in
      match fClosure with
      | ClosureInclude (fBody, fDeclEnv) -> 
          eval fBody fDeclEnv true te 
      | _ -> failwith "eval Call: not a function")
  | AccessTrust (ideTrust, ideVar) -> 
      if t then
        failwith "Tainted sources cannot access trust block"
      else(
        let (trustV, taintV) = eval ideTrust env t te in
        (match (trustV, taintV) with
         | (Block (list, secondEnv), taintV) ->
             eval ideVar secondEnv taintV list
         | _ -> failwith "the access must be applied to an trustblock"))
  | Assert (ide) -> (
      let taintness = taint_lookup env ide in
      if taintness then
        failwith ("Assertion Failed - Var \"" ^ ide ^ "\" is tainted")
      else
        (lookup env ide , taintness))


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

      
let print_eval (ris : value * bool) = (*Just to display on the terminal the evaluation result*)
  match ris with
  | (Int(u), t) -> Printf.printf "Result: Int %d, Taintness: %b\n" u t 
  | (Bool(u), t) -> Printf.printf "Result: Bool %b, Taintness: %b \n" u t
  | (String(u), t) -> Printf.printf "Result: String %s, Taintness: %b\n" u t
  | (Block(_,_), _) -> Printf.printf "Result: Block created succesfully\n" 
  | _ -> Printf.printf "Closure\n";;

(*------------------------------------------------------------------------*)
(*----------------------------------MAIN----------------------------------*)
(*------------------------------------------------------------------------*) 
  
let env = [];;
let list = ([],[],[]);; 

(*
  let test_let_and_prim = execWithFailure (
      Let("x", CstI 3, 
          Prim("*", Var("x"), CstI 8)
         )
    ) env list;;
  print_eval(test_let_and_prim) 
  
  let test_assign = execWithFailure (
      Assign("x", CstI 5)
    ) env list;;
  print_eval(test_assign)

  let test_if = execWithFailure (
      Let("x", CstI 3, 
          Let("y", CstI 5,
              If(Prim(">", Var("x"), Var("y")),
                 CstB true,
                 CstB false
                )
             )
         )
    ) env list;;

  print_eval(test_if)

  let test_fun_call = execWithFailure (
      Let(
        "sumXY", 
        Fun("x", 
            Fun("y",
                Let("x",
                    CstI 0,
                    Let("y",
                        CstI 1,
                        Prim("+", Var("x"), Var("y"))
                       )
                   ))), 
        Call(
          Call(Var("sumXY"),
               CstI 2), 
          CstI 0)
      )
    ) env list;;
  print_eval(test_fun_call) 

  let test_tBlock = eval (
      Assign("mytrustB", TrustBlock(
          LetSecret("x", CstI 1, 
                    LetPublic("funy", 
                              Fun("a", 
                                  Fun("b",
                                      Prim("+", Var("a"), Var("b"))
                                     )
                                 ),
                              Handle("funy", EndTrustBlock)))
        ) 
        )) env list;;
  print_eval(test_tBlock)

let test_Include_Execute = execWithFailure (
    Let("x", 
        Include(Let("a",
                    CstI 0,
                    Let("b",
                        CstI 10,
                        Assign("b", CstI 100)
                       )
                   )
               ),
        Let("z",
            Execute(Var("x")),
            Assign("z", Var("x"))
           )
       )
  ) env list;;
print_eval(test_Include_Execute)
  
let test_TU = execWithFailure(
    Let("mytrustB", 
        TrustBlock(LetSecret("x",
                             CstI 1,
                             LetPublic("y",
                                       CstI 3,
                                       Handle("y", EndTrustBlock)
                                      )
                            )
                  ),
        Let("extCode",
            Include(Let("a", 
                        CstI 5,
                        Let("b",
                            CstI 8,
                            Assign("b", 
                                   AccessTrust(Var("mytrustB"), 
                                               Var("x")
                                              )
                                  )
                           )
                       )
                   ), 
            Execute(Var("extCode")) 
           )
       )
  ) env list;;
print_eval(test_TU) *)

(*
let mytrustB = TrustBlock {
let secret x = 1;
let public y = 3;
handle y;
}

let plainCode =
let extCode = Include {
let a = 5;
let b = 8;
a * b;
}

Execute(extCode);
basd = mytrustB.y;
*)



(*questo test fallisce al momento della Execute perché cerco di chiamare y 
all'interno della Include*) 
let test_TU_1 = eval(
    Let("mytrustB", 
        TrustBlock(LetSecret("x",
                             CstI 1,
                             LetPublic("y",
                                       CstI 3,
                                       Handle("y", EndTrustBlock)
                                      )
                            )
                  ),
        Let("plainCode", 
            AccessTrust(Var("mytrustB"), Var("y")),
            Let("extCode",
                Include(Let("a", 
                            CstI 5,
                            Let("b",
                                AccessTrust(Var("mytrustB"), Var("y")),
                                Prim("*", Var("b"), Var("a"))
                               )
                           )
                       ),
                Execute(Var("extCode")) 
               )
           )
       )
  ) env false list;;
  
(*
  let test_TU = eval(
      Let("mytrustB", 
          TrustBlock(
            LetSecret("x",
                      CstI 1,
                      LetPublic(
                        "y",
                        CstI 3,
                        Handle("y", EndTrustBlock)
                      )
                     )),
          Let("plainCode",
              Let("extCode",
                  Include(Let("a", 
                              CstI 5,
                              Let("b",
                                  CstI 8,
                                  Prim("*", AccessTrust(Var("mytrustB"), Var("y")), Var("a"))
                                 )
                             )
                         ),
                  Execute(Var("extCode"))),
              Assign("basd", AccessTrust(Var("mytrustB"), Var("y"))) (*questo test stampa 10 con valore della taintness true, quindi è già la execute
               che fa il controllo sugli accessi, non la include. Quindi dobbiamo trovare un modo nella execute che se non alza errori
               alla fine deve ritornare false e non true*)
             )
         )
    )env false list;;
  print_eval(test_TU)*)
