open Stackinspection.Interpreter
open Stackinspection.Security

(*
  For testing purpose: test if the evaluation fails
*)
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
