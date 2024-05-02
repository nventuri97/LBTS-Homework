open homework_1.Interpreter
open homework_1.Security

(*
  let mycode= trust{
    let secret x=0
    let sum= fun 2 + 3
    handle sum
}
*)
exec_without_failure
Trust_block(
  "MyCode",
  Let_Secret("x", CstI 0),
  Let_Public(
    "sum",
    Fun(
      Prim("+", CstI 2, CstI 3)
    )
  ),
  Handle("sum")
);
(*
  let mycode1= trust{
    let secret x=0
    let f_sum p1 = fun 2 + p1
    f_sum x
  } 
Error: you cant modify secret value, even inside a trust block
*)

exec_with_failure
Trust_block(
  "MyCode1",
  Let_Secret("x", CstI 0),
  Let_Public(
    "f_sum",
    Fun(
      "p1",
      Prim("+", CstI 2, Var "p1")
    )
  )
  Call(Var "f_sum", Var"x")
);

(*
let inc_fun = include{
  let x=2
  let y=3
  let mult p1 p2= fun p1*p2
}
execute inc_fun;
*)
exec_without_failure
Include(
  Let("x", CstI 2),
  Let("y", CstI 3),
  Let("mult",
    Fun(
      "p1", "p2", 
      Prim("*", Var "p1", Var"p2"))
    )
)
Execute("external");

(*Parte della dynamic tainted analysis*)

(*
MAIN del professore, per ora lasciamolo qua
  For testing purpose: test if the evaluation fails

let execWithFailure test env stack =
  let value = try eval test env stack with Failure _ -> Int 1 in
  assert (value = Int 1)

(*
  For testing purpose: test if the evaluation does not fail
*)
let execWithoutFailure test env stack =
  let value = try eval test env stack with Failure _ -> Int 0 in
  assert (value <> Int 0)

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

let () = execute_examples examples
*)