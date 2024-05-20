open Homework_1.Interpreter

let env = [];;
let list = ([],[],[]);;

let execWithFailure test env t list = 
    let (value, success) =
      try 
        eval test env t list 
      with 
        Failure msg -> 
          Printf.printf "Test fallito con errore: %s\n" msg;
          (Bool false, false)
    in
    assert (value = Bool false && success = false);;

  print_string "Test_0\n";;
  let test_TU = eval(
    Let("mytrustB", 
        TrustBlock(LetSecret("x",
                             CstI 1,
                             LetPublic("y",
                                       CstI 3,
                                       Handle("y", EndTrustBlock)
                                      )
                            )
                  ),
        Let("plainCode",
            Let("extCode",
                Include(Let("a", 
                            CstI 5,
                            Let("b",
                                CstI 5,
                                Prim("*", Var("b"), Var("a"))
                               )
                           )
                       ),
                Assign("plainCode",
                       Execute(Var("extCode"))
                      )
               ),
            AccessTrust(Var("mytrustB"), Var("y"))
           )
       )
  ) env false list;;
  print_eval(test_TU);;

(*questo test stampa (6, false) e mi torna perché l'ultimo valore è untainted*) 
print_string "Test_1\n";;
let test_TU_1 = eval(
    Let("mytrustB", 
        TrustBlock(LetSecret("x",
                             CstI 1,
                             LetPublic("y",
                                       CstI 3,
                                       Handle("y", EndTrustBlock)
                                      )
                            )
                  ),
        Let("plainCode",
            Let("extCode",
                Include(Let("a", 
                            CstI 5,
                            Let("b",
                                CstI 2,
                                Prim("*", Var("b"), Var("a"))
                               )
                           )
                       ),
                Assign("plainCode",
                       Execute(Var("extCode"))
                      )
               ),
            CstI 6
           )
       )
  ) env false list;;
 
  print_eval(test_TU_1);;
(*questo test stampa (3, false) e mi torna perché lo stesso y non è stata toccata
da nessun valore tainted*)
print_string "Test_2\n";;
let test_TU_2 = eval(
    Let("mytrustB", 
        TrustBlock(LetSecret("x",
                             CstI 1,
                             LetPublic("y",
                                       CstI 3,
                                       Handle("y", EndTrustBlock)
                                      )
                            )
                  ),
        Let("plainCode",
            Let("extCode",
                Include(Let("a", 
                            CstI 5,
                            Let("b",
                                CstI 2,
                                Prim("*", Var("b"), Var("a"))
                               )
                           )
                       ),
                Assign("plainCode",
                       Execute(Var("extCode"))
                      )
               ),
            AccessTrust(Var("mytrustB"), Var("y"))
           )
       )
  ) env false list;;
  print_eval(test_TU_2);;
(*questo test stampa (10, true) e mi torna perché l'ultimo valore valutato è 
5*2 nella execute, quindi tainted*)
print_string "Test_3\n";; 
let test_TU_3 = eval(
    Let("mytrustB", 
        TrustBlock(LetSecret("x",
                             CstI 1,
                             LetPublic("y",
                                       CstI 3,
                                       Handle("y", EndTrustBlock)
                                      )
                            )
                  ),
        Let("plainCode", 
            AccessTrust(Var("mytrustB"), Var("y")),
            Let("extCode",
                Include(Let("a", 
                            CstI 5,
                            Let("b",
                                CstI 2,
                                Prim("*", Var("b"), Var("a"))
                               )
                           )
                       ),
                Execute(Var("extCode")) 
               )
           )
       )
  ) env false list;;
  print_eval(test_TU_3);;
(*questo test fallisce al momento della Execute perché cerco di chiamare y 
all'interno della Include *)
print_string "Test_4\n";;
execWithFailure (
    Let("mytrustB", 
        TrustBlock(LetSecret("x",
                             CstI 1,
                             LetPublic("y",
                                       CstI 3,
                                       Handle("y", EndTrustBlock)
                                      )
                            )
                  ),
        Let("plainCode", 
            AccessTrust(Var("mytrustB"), Var("y")),
            Let("extCode",
                Include(Let("a", 
                            CstI 5,
                            Let("b",
                                AccessTrust(Var("mytrustB"), Var("y")),
                                Prim("*", Var("b"), Var("a"))
                               )
                           )
                       ),
                Execute(Var("extCode")) 
               )
           )
       )) env false list;;

  print_string "Test_5\n";;
  let test_TU_5 = eval(
    Let("mytrustB", 
        TrustBlock(LetSecret("x",
                             CstI 1,
                             LetPublic( "f",
                              Fun ("z", Prim ("*", Var "z", CstI 5)),
                             Handle ("f", EndTrustBlock) )
                            )
                  ),
        Call(AccessTrust(Var("mytrustB"), Var("f")), CstI 2)
       )
  ) env false list;;
  print_eval(test_TU_5);;
(*questa deve fallire*)
print_string "Test_6\n";;
execWithFailure(
    Let("mytrustB", 
        TrustBlock(LetSecret("x",
                             CstI 1,
                             LetPublic( "f",
                              Fun ("z", Prim ("*", Var "z", CstI 5)),
                             Handle ("f", EndTrustBlock) )
                            )
                  ),
        Let("extCode",
            Include(Let("a", 
                      CstI 5,
                      Call(AccessTrust(Var("mytrustB"), Var("f")), Var("a"))
                             )
                         ),
                  Execute(Var("extCode")) 
                 )
       )
  ) env false list;;

  (*TrustedVar used inside a trust block*)
  print_string "Test_7\n";;
  let test_TU_7 = eval(
    Let("mytrustB", 
        TrustBlock(LetPublic("x",
                             CstI 11,
                             LetPublic( "f",
                                TrustedVar("x"),
                             Handle ("f", EndTrustBlock) )
                            )
                  ),
        AccessTrust(Var("mytrustB"), Var("f"))
       )
  ) env false list;;
  print_eval(test_TU_7);;

(*Cant declare handle a secret*)
print_string "Test_8\n";;
execWithFailure(
    Let("mytrustB", 
        TrustBlock(LetSecret("x",
                             CstI 11,
                             LetPublic( "f",
                             TrustedVar("x"),
                             Handle ("x", EndTrustBlock) )
                            )
                  ),
        AccessTrust(Var("mytrustB"), Var("x"))
       )
) env false list;;

(*TrusteVar outside a trustBlock*)
print_string "Test_9\n";;
execWithFailure(
    Let("mytrustB", 
        TrustBlock(LetPublic("x",
                             CstI 11,
                             LetPublic( "f",
                             TrustedVar("x"),
                             Handle ("f", EndTrustBlock) )
                            )
                  ),
        Assign("PlainT", TrustedVar("x"))
       )
) env false list;;

(*trustBlock inside an include*)
print_string "Test_10\n";;
execWithFailure(
    Let("myUtrustB", 
        Include(TrustBlock(LetSecret("x", CstI 10, EndTrustBlock))
                  ),
        Execute(Var"myUtrustB")
        )
) env false list;;

    (*include inside an include*)
print_string "Test_11\n";;
execWithFailure(
    Let("myUtrustB", 
        Include(Include(Let("a", 
                            CstI 5,
                            Prim("*", Var "a", CstI 8))
                        )
                      ),
        Execute(Var"myUtrustB")
        )
) env false list;;

(*This should work but we are multipling a tainted value with an untainted one, still no trust block involved*)
print_string "Test_12\n";;
let test_TU_12 = eval(
  Let("mytrustB", 
      TrustBlock(LetSecret("x",
                           CstI 1,
                           LetPublic("y",
                                     CstI 3,
                                     Handle("y", EndTrustBlock)
                                    )
                          )
                ),
      Let("plainCode",
          Let("extCode",
              Include(Let("a", 
                          CstI 2,
                          Let("b",
                              CstI 5,
                              Prim("*", Var("b"), Var("a"))
                             )
                         )
                     ),
              Assign("plainCode",
                     Execute(Var("extCode"))
                    )
             ),
          Let("g", CstI 10, Prim("*", Var "g", Var "plainCode"))
         )
     )
) env false list;;
print_eval(test_TU_12);;

(* Testing Assert in normal code *)
print_string "Test_13\n";;
let test_TU_13 = eval(
  Let("code",
    Let("x", 
      CstI 2,
      Prim("+", Var "x", CstI 2)
    ),
    Assert("code")
  )
) env false list;;
print_eval(test_TU_13);;

(* Testing Assert on Untrusted Block *)
print_string "Test_14\n";;
execWithFailure(
  Let("plugin",
    Include(
      Let("x", CstB false,
          Let("y", CstB true,
          Prim("&&", Var("x"), Var("y"))
        )
      )
    ),
    Let("exe",
      Execute(Var("plugin")),
      Assert("exe")
    )
  )
) env false list;;