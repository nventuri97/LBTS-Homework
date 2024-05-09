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
  
(*List that contains all the identificators definied in a trust block to understand
   if an indentificator is defined in a trust block or not*)
type trusted = ide list

   (*Sublist of trusted that contains the identificators of secret variable or function*)
type secret = ide list
   
   (*Sublist of trusted that contains the indentificators of the function defined 
      as handle within a trustblock*)
type handleList = ide list

type 'v trustedEnv = trusted * secret * handleList
   
let rec isIn (x: 'v) (l: 'v list) : bool =
  match l with
  | [] -> false
  | head::tail -> if x=head then true else isIn x tail
             
let build (t: trusted) (s: secret) (h: handleList) : 'v trustedEnv = (t, s, h)

let getTrust (e : 'v trustedEnv) : trusted =
  let trustedL, _, _ = e in
  trustedL
  
  (*
    Get the gateways from a trusted environment.
  *)
let getHandle (e :  'v trustedEnv) : handleList =
  let _, handle, _ = e in
  handle
  
  (*
    Get the secrets from a trusted environment.
  *)
let getSecret (e : 'v trustedEnv) : secret =
  let _, _, secr = e in
  secr


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
  | Include of ide * expr
  | Execute of expr * expr
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
  | Block of value trustedEnv
        
        
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
  | EndTrustBlock -> Block(te)

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
  | Include (_, _) -> failwith "Not yet implemented"
  | Execute (_, _) -> failwith "Not yet implemented"
      (* match eval e1 env with
      | TrustBlock (_,_) -> failwith "Not yet implemented"
      | Include (_, _, _) -> failwith "Not yet implemented"
      | _ -> failwith "Impossible to execute" *)



let print_ide_list ide_list = 
  List.iter (fun ide -> print_string ide; print_string " ") ide_list;
  print_newline ()
    
let print_trustedEnv (env : 'v trustedEnv) =
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
  | Block(u) -> print_trustedEnv u
  | _ -> Printf.printf "Closure\n";;

(*
let mycode= trust{
    let secret x=0
    let sum p1 p2= p1+p2 
    handle sum
}

 TrustBlock(
  "MyCode",
  Let_Secret("x", CstI 0),
  Let_Public(
    "sum",
    Fun(
      "p1", "p2",
      Prim("+", Var("p1"), Var("p2"))
    )
  ),
  Handle("sum"),
EndTrustBlock
);
  let mycode1= trust{
    let secret x=0
    handle x
} 
Error: you cant handle a secret information
TrustBlock(
  "MyCode1",
  Let_Secret("x", CstI 0),
  Handle("x"),
  EndTrustBlock
);

(*
let inc_fun = include{
  let x=2
  let y=3
  let mult p1 p2= p1*p2
}
execute(inc_fun.mult, 3, 2);
*)
Include(
  "inc_fun",
  Let("x", CstI 2),
  Let("y", CstI 3),
  Let("mult",
    Fun(
      "p1", "p2", 
      Prim("*", Var "p1", Var"p2"))
    )
Execute("inc_fun.mult", CstI 3, CstI 4);
) 
  *)
let execWithFailure test env =
  try
    let result = eval test env in
      (* Convertire il risultato in value per usare print_eval *)
    result
  with Failure msg -> 
    String ("Error: " ^ msg)
  
let env = [];;

let test_let_and_prim = eval (
    Let("x", CstI 3, 
        Prim("*", Var("x"), CstI 8)
       )
  ) env ;;
print_eval(test_let_and_prim) 
  
let test_assign = eval (
    Assign("x", CstI 5)
  ) env ;;
print_eval(test_assign)

let test_if = eval (
    Let("x", CstI 3, 
        Let("y", CstI 5,
            If(Prim(">", Var("x"), Var("y")),
               CstB true,
               CstB false
              )
           )
       )
  ) env ;;

print_eval(test_if)

let test_fun_call = eval (
    Let(
      "sumXY", 
      Fun("x", 
          Fun("y",
              Prim("+", Var("x"), Var("y"))
             )
         ), 
      Call(
        Call(Var("sumXY"),
             CstI 2), 
        CstI 0)
    )
  ) env ;;

let example = eval (
    Let("x", CstI 3, 
        Prim("*", Var("x"), CstI 8)
       )
  ) env;;
print_eval(example)


let example1 = execWithFailure (
    Let("x", CstI 3, 
        Prim("*",  Var("x"), Var("y"))
       )
  ) env;;
print_eval(example1)

let example2 = eval (
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
      )) env;;
print_eval(example2)

(*
let example3 = eval (
  Let(
    "mytrustB", 
    TrustBlock(
      LetSecret("x", CstI 1, 
       LetPublic("y", CstI 1,
        Handle("y", EndTrustBlock)))
    ),
      Include(
        "fun",Assign("x", CstI 0), 
      )
    )
  )env;;
print_eval(example3)

let example1 = eval(
NewLet("myCode",
  TrustBlock(
    LetSecret("x", CstI 0),
    LetPublic("sum",
      Fun(
        "p1", "p2",
        Prim("+", Var("p1"), Var("p2"))
      )
    ),
    Handle("sum", EndTrustBlock)
  )
)
)env stack;; 
print_eval(example1);;*)
