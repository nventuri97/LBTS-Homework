open homework_1.Interpreter
open homework_1.Security

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
      execute_examples t

let () = execute_examples examples
*)
