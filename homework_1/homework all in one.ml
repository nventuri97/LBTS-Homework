
(*
    Return true if l1 is sublist of l2
*)
let rec sublist l1 l2 =
  match l1 with
  | [] -> true
  | e :: l -> if List.mem e l2 then sublist l l2 else false

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
  | TrustBlock of trust_content
  | Include of ide * expr * expr
  | Execute of expr * expr
and trust_content =
  | LetSecret of ide * expr * trust_content
  | LetPublic of ide * expr * trust_content
  | Handle of ide * trust_content
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


type stack = value list
    

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
  | TrustBlock (_) -> failwith "Not yet implemented"
      (* Aggiungere la logica per gestire TrustBlock qui *)
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



(* TrustBlock(
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
(*
  let mycode1= trust{
    let secret x=0
    handle x
} 
Error: you cant handle a secret information
*)
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
) *)


(*Parte della dynamic tainted analysis*)


(* MAIN del professore, per ora lasciamolo qua
  For testing purpose: test if the evaluation fails *)

(* let execWithFailure test env stack =
  let value = try eval test env stack with Failure _ -> Int 1 in
  assert (value = Int 1) *)

(*
  For testing purpose: test if the evaluation does not fail
(* *)
let execWithoutFailure test env t stack =
   eval test env t stack ;; *)

let env = [];;
let stack = [];;

let test_let_and_prim = eval (
    Let("x", CstI 3, 
        Prim("*", Var("x"), CstI 8)
       )
  ) env stack;;
print_eval(test_let_and_prim) 
  
let test_assign = eval (
    Assign("x", CstI 5)
  ) env stack;;
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
  ) env stack;;

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
  ) env stack;;

  
   (*
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
print_eval(example1);;


let examples =
  [
    execWithFailure
      (SecLet
         ( "f",
           Fun
             ( "x",
               Prim ("+", Var "x", CstI 1),
               [ Permission ("File", "f1.txt", [ "w" ]) ] ),
           [
             Permission ("File", "f1.txt", [ "w" ]);
             Permission ("File", "f2.txt", [ "w"; "r" ]);
           ],
           Let
             ( "_",
               CheckPermission (Permission ("File", "f1.txt", [ "r" ])),
               Call (Var "f", CstI 2) ) ))
      [] [];
  ]

let rec execute_examples ex =
  print_endline "Running test case";
  match ex with
  | [] -> print_endline "Done"
  | x :: t ->
      x;
      execute_examples t

let () = execute_examples examples *)

