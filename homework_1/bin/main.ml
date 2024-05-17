open Homework_1.Interpreter

(* let execWithFailure test env list=
  try
    let result = eval test env list in
      (* Convertire il risultato in value per usare print_eval *)
    result
  with Failure msg -> 
    String ("Error: " ^ msg) *)
  
let env = [];;
let list = ([],[],[]);; 

(* let test_let_and_prim = execWithFailure (
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
    Let("y", 
        CstI 54,
        Let("x", 
            Include(Let("a",
                        CstI 0,
                        Let("b",
                            CstI 10,
                            Assign("a", CstI 100)
                           )
                       )
                   ),
        
            Call(Var("y"), Var("x"))
           )
       )
  ) env list;;
print_eval(test_Include)


let mytrust = trust{
  let secret x= 0;
  let public sum =x;
  let public h= 10;
  handle sum; 
}
let myInclude = include{
  let c=1;
  let d=3;
  execute(sum, c, d);
}  


let test_tBlock1 = eval (
    Let("mytrustB", TrustBlock(
        LetSecret("x", 
                  CstI 1,
                  LetPublic("funy", 
                            CstI 8,
                            Handle("funy", EndTrustBlock) )
                            )
                  ), 
        Let("kal", 
          CstI 4 , 
          Prim("*", Var("kal"), AccessTrust(Var("mytrustB"), Var("x"))))
        )
      ) env list;; (*questo non fa fare l'accesso ne se chiami x ne se chiami funy*)
print_eval(test_tBlock1)*)
(*
let test2 = eval(
Let("mytrustB", 
    TrustBlock(
      LetSecret("x",
                CstI 1,
                LetPublic(
                  "y",
                  CstI 3,
                  Handle("y", EndTrustBlock)
                )
      )),
      Let("a", 
          CstI 5,
          Let("b",
              CstI 8,
              Prim("*", Var("b"), AccessTrust(Var("mytrustB"), Var("y"))
              )
            )
          )
    )
  ) env false list;;
print_eval(test2) 


(* Testing Assert on Trusted Block *)
let test2 = eval(
  Let("mytrustB", 
    TrustBlock(
      LetSecret("x", CstI 1,
          LetPublic("y", CstI 3,
              Handle("y", EndTrustBlock)
          )
      )
    ),
    Let("a", AccessTrust(Var("mytrustB"), Assert("y")), Assert("a"))
  )
 ) env false list;;
print_eval(test2)

(* Testing Include & Execute *)
let test1 = eval(
  Let("extCode",
    Include(
      Let("a", CstI 5,
        Let("b", CstI 8,
          Prim("*", Var("a"), Var("b"))
        )
      )
    ),
    Execute(Var("extCode"))
  )
 ) env false list;;
print_eval(test1)

(* Testing Assert on Untrusted Block *)
let test2 = eval(
  Let("plugin", 
    Include(
      Let("x", CstB false,
          Let("y", CstB true,
          Prim("&&", Var("x"), Var("y"))
        )
      )
    ),
    Execute(Assert("plugin"))
  )
 ) env false list;;
print_eval(test2)

(* Testing Include & Execute on Trusted Handled values *)
let test2 = eval(
  Let("mytrustB", 
      TrustBlock(
        LetSecret("x", CstI 1,
            LetPublic("y", CstI 3,
                Handle("y", EndTrustBlock)
            )
        )
      ),
      Let("extCode",
        Include(
          Let("a", CstI 5,
            Let("b", CstI 8,
              Prim("*", AccessTrust(Var("mytrustB"), Var("y")), Var("b"))
            )
          )
        ),
        (* Notice that the fail happens here and not inside the Include block *)
        Execute(Var("extCode")) 
      )
    )
 ) env false list;;
print_eval(test2)
*)
let test_TU = eval(
  Let("mytrustB", 
      TrustBlock(
        LetSecret("x",
                  CstI 1,
                  LetPublic(
                    "y",
                    CstI 3,
                    Handle("y", EndTrustBlock)
                  )
        )),
        Let("plainCode",
          Let("extCode",
              Include(Let("a", 
                          CstI 5,
                          Let("b",
                              CstI 8,
                              Prim("*", Var("b"), Var("a"))
                              )
                          )
                        ),
              Execute(Var("extCode"))),
          Assign("basd", AccessTrust(Var("mytrustB"), Var("y"))) (*questo test stampa 10 con valore della taintness true, quindi è già la execute
             che fa il controllo sugli accessi, non la include. Quindi dobbiamo trovare un modo nella execute che se non alza errori
             alla fine deve ritornare false e non true*)
            )
        )
      )env false list;;
  print_eval(test_TU)
  let test_TU = eval(
    Let("mytrustB", 
        TrustBlock(
          LetSecret("x",
                    CstI 1,
                    LetPublic(
                      "y",
                      CstI 3,
                      Handle("y", EndTrustBlock)
                    )
          )),
          Let("plainCode",
            Let("extCode",
                Include(Let("a", 
                            CstI 5,
                            Let("b",
                                CstI 8,
                                Prim("*", AccessTrust(Var("mytrustB"), Var("y")), Var("a"))
                                )
                            )
                          ),
                Execute(Var("extCode"))),
            Assign("basd", AccessTrust(Var("mytrustB"), Var("y"))) (*questo test stampa 10 con valore della taintness true, quindi è già la execute
               che fa il controllo sugli accessi, non la include. Quindi dobbiamo trovare un modo nella execute che se non alza errori
               alla fine deve ritornare false e non true*)
              )
          )
        )env false list;;
    print_eval(test_TU)