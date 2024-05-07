open Homework_1.Interpreter
(* open Homework_1.Security *)
(*
let execWithFailure test env stack =
  let value = try eval test env stack with Failure _ -> Int 1 in
  assert (value = Int 1)

(*
  For testing purpose: test if the evaluation does not fail
*)
let execWithoutFailure test env stack =
  let value = try eval test env stack with Failure _ -> Int 0 in
  assert (value <> Int 0)


  let mycode= trust{
    let secret x=0
    let sum p1 p2= p1+p2 
    handle sum
}
*)

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
) 
  *)
  let execWithFailure test env stack =
    try
      let result = eval test env stack in
      (* Convertire il risultato in value per usare print_eval *)
      result
    with Failure msg -> 
      String ("Error: " ^ msg)
  
let env = [];;
let stack = [];;

let example = eval (
 Let("x", CstI 3, 
    Prim("*", Var("x"), CstI 8)
    )
    ) env stack;;
print_eval(example)


let example1 = execWithFailure (
  Let("x", CstI 3, 
  Prim("*",  Var("x"), Var("y"))
  )
  ) env stack;;
print_eval(example1)

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
