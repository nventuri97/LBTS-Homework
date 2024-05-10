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
type 'v env = (ide * 'v) list

(*To extend an environment*)
let extend (e : 'v env) (id : ide) (v : 'v) : 'v env = (id, v) :: e
(*
  Given an environment {env} and an identifier {x} it returns the data {x} is bound to.
  If there is no binding, it raises an exception.
*)
let rec lookup env x =
  match env with
  | [] -> failwith (x ^ " not found")
  | (y, v) :: r -> if x = y then v else lookup r x

let rec taint_lookup env x=
  match env with
  | [] -> failwith (x ^ " not found")
  | (y, _, t)::r -> if x = y then t else taint_lookup r x 
  
          
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
  | GetInput of expr    (*functions that takes input, taint source*)
  | TrustBlock of trustContent
  | Include of expr
  | Execute of expr
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
  | Block of string
        
        
(*------------------------------------------------------------------------*)
(*------------------------------INTERPRETER-------------------------------*)
(*------------------------------------------------------------------------*)

let rec evalTrustContent (tc : trustContent) (env : value env) (te : value trustedList)
    (eval :
       expr ->
     value env->
     value trustedList ->
     value) : value =
  match tc with
  | LetSecret (id, exprRight, next) ->
      let addsec = id :: (getSecret te) in
      let newTrustList = build (getTrust te) addsec (getHandle te)  in
      let id_value = eval exprRight env te in
      let newEnv = extend env id id_value in
      evalTrustContent next newEnv newTrustList eval
  | LetPublic (id, exprRight, next) ->
      let addtrus = id :: (getTrust te) in
      let newTrustList = build addtrus (getSecret te) (getHandle te)  in
      let id_value = eval exprRight env te in
      let newEnv = extend env id id_value in
      evalTrustContent next newEnv newTrustList eval
  | Handle (id, next) ->  (*aggiungere il caso in cui quello che chiama la id non utilizzi cose trusted*)
      if isIn id (getSecret te) then failwith "can't declare handle a secret"
      else if isIn id (getTrust te) then 
        let addhandle = id::(getHandle te) in
        let newTrustList = build (getTrust te) (getSecret te) addhandle in
        evalTrustContent next env newTrustList eval
      else failwith "can't add to handle list a variable not trusted"
  | EndTrustBlock -> Block("TrustBlock created with success!") 
                       

let rec eval (e : expr) (env : value env) (te : value trustedList): value =
  match e with
  | CstI i -> Int i
  | CstB b -> Bool (if b then true else false)
  | CstString s -> String s
    (* | Var (x, _) -> Value (lookup env x, taint_lookup env x) *)
  | Var (x) -> lookup env x
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
      let newList = build [] [] [] in (*gli passo 3 liste come quelle che abbiamo usato in security*) 
      evalTrustContent tc env newList eval
  | Include (iBody) -> (ClosureInclude (iBody, env))
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

(*------------------------------------------------------------------------*)
(*----------------------------------MAIN----------------------------------*)
(*------------------------------------------------------------------------*)

let execWithFailure test env list=
  try
    let result = eval test env list in
      (* Convertire il risultato in value per usare print_eval *)
    result
  with Failure msg -> 
    String ("Error: " ^ msg)
  
let env = [];;
let list = ([],[],[]);; 

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

let test_tBlock = execWithFailure (
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

let test_Include = execWithFailure (
    Let("x", 
        Include(Let("a",
                    CstI 0,
                    Let("b",
                        CstI 10,
                        Assign("b", CstI 100)
                       )
                   )
               ),
        
        Execute(Var("x"))
       )
  ) env list;;
print_eval(test_Include)
