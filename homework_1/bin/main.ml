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

let example = eval 
  (Let("x", CstI 3, 
      Prim("+", Var("x"), CstI 1)
      )
  ) env stack;;
print_eval(example)

(* let examples =
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
    execWithFailure (ReadFile "f1.txt") []
      [
        Grant
          [
            Permission ("File", "f1.txt", [ "w" ]);
            Permission ("File", "f2.txt", [ "w"; "r" ]);
          ];
      ];
    execWithoutFailure
      (SecBlock
         ( Grant [ Permission ("File", "f1.txt", [ "w" ]) ],
           SendFile (CstI 42, "f1.txt") ))
      []
      [ Grant [ Permission ("File", "f1.txt", [ "w"; "r" ]) ] ];
    execWithoutFailure
      (SecBlock
         ( Grant [ Permission ("File", "f1.txt", [ "w" ]) ],
           SendFile (CstI 42, "f1.txt") ))
      []
      [ Grant [ Permission ("File", "*", [ "w" ]) ] ];
    execWithFailure
      (Enable (Permission ("File", "f1.txt", [ "r" ]), ReadFile "f1.txt"))
      [] [];
    execWithFailure
      (Enable (Permission ("File", "*", [ "w" ]), ReadFile "f1.txt"))
      [] [];
    execWithoutFailure
      (Enable
         ( Permission ("File", "f1.txt", [ "w"; "r" ]),
           SendFile (CstI 42, "f1.txt") ))
      [] [];
    execWithFailure
      (Disable
         (Permission ("File", "f1.txt", [ "w" ]), SendFile (CstI 42, "f1.txt")))
      []
      [ Grant [ Permission ("File", "*", [ "w" ]) ] ];
  ]

let rec execute_examples ex =
  print_endline "Running test case";
  match ex with
  | [] -> print_endline "Done"
  | x :: t ->
      x;
      execute_examples t

let () = execute_examples examples *)
