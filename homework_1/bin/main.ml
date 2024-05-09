open Homework_1.Interpreter
(* open Homework_1.Security *)

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
      LetPublic("y", CstI 1,
        Handle("y", EndTrustBlock)))
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
        Handle("y", EndTrustBlock)))),
      Inlcude(
        "z", 
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