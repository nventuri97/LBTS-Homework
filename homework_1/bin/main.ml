open Interpreter
open Security

let execWithFailure test=
  let value = try eval test with Failure _ -> Int 1 in
  assert (value = Int 1);;

let execWithoutFailure test=
  let value = try eval test with Failure _ -> Int 0 in
  assert (value <> Int 0);;

let test_1 = Trust("Mycode","Felix");;
let test_2= Include("Mycode", "Felix", Secret, (*Operazione1 segreta da aggiungere*));;
execWithoutFailure test_2;; (*da capire come collegare il risultato della eval a exec without e with failure*)
let test_3= Include("Mycode", "", Public, (*Operazione2 publica da aggiungere*));;
execWithoutFailure test_3;; (*non deve fallire*)

let test_4= Include("Mycode", "", Secret, (*Operazione, non è importante quale visto che fallisce*));;
execWithFailure test_4;; (*DEVE fallire perchè stiamo creando un segreto senza mettere la password*)

let test_5= Include("Mycode", "Sad", Secret, (*Operazione segreta da aggiungere*));;
execWithFailure test_5;; (*Deve fallire perchè la password è sbagliata*)

let test_6= Execute("Mycode", "Felix", (*Operazione1*));;
execWithoutFailure test_6;; (*non deve fallire*)
let test_7= Execute("Mycode", "", (*Operazione2*));;
execWithoutFailure test_7;; (*non deve fallire perchè è pubblica *)
let test_8= Execute("Mycode", "", (*Operazione1*));;
execWithFailure test_8;; (*deve fallire perchè l'operazione1 è dichiarata segreta e non ho messo la password *)
let test_9= Execute("Mycode", "Sad", (*Operazione1*));;
execWithFailure test_9;; (* deve fallire per password sbagliata*)
let test_10= Execute("Mycode1", "", (*Operazione2*));;
execWithFailure test_10;; (*deve fallire perchè non esiste un trust mycode1*)

let test_11= Trust("Mycode", "Rand");;
execWithFailure test_11;; (* deve fallire perchè esiste già un trust con quel nome*)

let t1 =
  let secret (x,y) =
    (*Calculate the position, in an hidden way*)
  in Trust("MyPosition", t1)

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