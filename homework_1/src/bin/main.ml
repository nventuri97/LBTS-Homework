open Homework_1.Interpreter

let env = []
let list = ([], [], [])

let execWithFailure test env t list =
  let value, success =
    try eval test env t list
    with Failure msg ->
      Printf.printf " Test Failed with exception:\027[31m %s\n\027[0m" msg;
      (Bool false, false)
  in
  assert (value = Bool false && success = false)
;;

let print_separator() =
  print_string "-------------------------------------------------------------------------------------\n"
;;

(* Accessing public trusted value y inside a normal code block, in which we also have a plugin that doesnt use trusted values *)
print_separator();;
print_string " Test_0\n"
let test_TU = eval(
    Let("mytrustB",
        TrustBlock(
            LetSecret("x",
                CstI 1,
                LetPublic("y",
                    CstI 3,
                    Handle("y",
                        EndTrustBlock)
                )
            )
        ),
        Let("plainCode",
            Let("extCode",
                Include(
                    Let("a",
                        CstI 5,
                        Let("b",
                            CstI 5,
                            Prim ("*", Var "b", Var "a")
                        )
                    )
                ),
                Assign("plainCode", Execute(Var "extCode"))
            ),
            AccessTrust(Var "mytrustB", Var "y")
        )
    )
) env false list;;
print_eval test_TU;;
print_separator();;

(* Same as before, but this time we show the taintness of the included code *)
print_string " Test_1\n"
let test_TU_1 = eval(
    Let("mytrustB",
        TrustBlock(
            LetSecret("x",
                CstI 1,
                LetPublic("y",
                    CstI 3,
                    Handle("y", EndTrustBlock)
                )
            )
        ),
        Let("plainCode",
            Let("extCode",
                Include(
                    Let("a",
                        CstI 5,
                        Let ("b",
                            CstI 2,
                            Prim ("*", Var "b", Var "a")
                        )
                    )
                ),
                Assign("plainCode", Execute(Var "extCode"))
            ),
            Var("plainCode")
        )
    )
) env false list;;
print_eval test_TU_1;;
print_separator();;

(* Testing Execute on Include Block *)
print_string " Test_2\n"
let test_TU_2 = eval(
    Let("mytrustB",
        TrustBlock(
            LetSecret("x",
                CstI 1,
                LetPublic("y",
                    CstI 3,
                    Handle("y", EndTrustBlock)
                )
            )
        ),
        Let("plainCode",
            AccessTrust(Var "mytrustB", Var "y"),
            Let("extCode",
                Include(
                    Let("a",
                        CstI 5,
                        Let("b",
                            CstI 2,
                            Prim ("*", Var "b", Var "a")
                        )
                    )
                ),
                Execute (Var "extCode")
            )
        )
    )
) env false list;;
print_eval test_TU_2;;
print_separator();;

(* Executing a plugin code that tries to access trusted values *)
print_string " Test_3\n";;
execWithFailure(
    Let("mytrustB",
        TrustBlock(
            LetSecret("x",
                CstI 1,
                LetPublic ("y",
                    CstI 3,
                    Handle ("y", EndTrustBlock)
                )
            )
        ),
        Let("plainCode",
            AccessTrust(Var "mytrustB", Var "y"),
            Let("extCode",
                Include(
                    Let("a",
                        CstI 5,
                        Let("b",
                            AccessTrust (Var "mytrustB", Var "y"),
                            Prim ("*", Var "b", Var "a")
                        )
                    )
                ),
               Execute (Var "extCode")
            )
        )
    )
) env false list;;
print_separator();;

(* Calling a trusted public function *)
print_string " Test_4\n"
let test_TU_4 = eval(
    Let("mytrustB",
        TrustBlock(
            LetSecret("x",
                CstI 1,
                LetPublic("f",
                    Fun ("z",
                        Prim ("*", Var "z", CstI 5)
                    ),
                    Handle ("f", EndTrustBlock)
                )
            )
        ),
        Call (AccessTrust (Var "mytrustB", Var "f"), CstI 2)
    )
) env false list;;
print_eval test_TU_4;;
print_separator();;

(* Calling a trusted public function inside a plugin *)
print_string " Test_5\n";;
execWithFailure(
    Let("mytrustB",
        TrustBlock(
            LetSecret("x",
                CstI 1,
                LetPublic("f",
                    Fun("z",
                        Prim ("*", Var "z", CstI 5)
                    ),
                    Handle("f", EndTrustBlock)
                )
            )
        ),
        Let("extCode",
            Include(
                Let("a",
                    CstI 5,
                    Call(AccessTrust (Var "mytrustB", Var "f"), Var "a")
                )
            ),
            Execute(Var "extCode")
        )
    )
) env false list;;
print_separator();;

(* TrustedVar used inside a trust block *)
print_string " Test_6\n"
let test_TU_6 = eval(
    Let("mytrustB",
        TrustBlock(
            LetPublic("x",
                CstI 11,
                LetPublic("f",
                    TrustedVar "x",
                    Handle("f", EndTrustBlock))
            )
        ),
        AccessTrust(Var "mytrustB", Var "f")
    )
) env false list;;
print_eval test_TU_6;;
print_separator();;

(* Cant declare handle of a secret value *)
print_string " Test_7\n";;
execWithFailure(
    Let("mytrustB",
        TrustBlock(
            LetSecret("x",
                CstI 11,
                LetPublic("f",
                    TrustedVar "x",
                    Handle("x", EndTrustBlock)
                )
            )
        ),
        AccessTrust (Var "mytrustB", Var "x")
    )
) env false list;;
print_separator();;

(* TrustedVar outside a trustBlock*)
print_string " Test_8\n";;
execWithFailure(
    Let("mytrustB",
        TrustBlock(
            LetPublic("x",
                CstI 11,
                LetPublic("f",
                    TrustedVar "x",
                    Handle("f", EndTrustBlock)
                )
            )
        ),
        Assign ("PlainT", TrustedVar "x")
    )
) env false list;;
print_separator();;

(* Trying to create a TrustBlock inside an Include *)
print_string " Test_9\n";;
execWithFailure(
    Let("myUtrustB",
        Include(
            TrustBlock(
                LetSecret("x", CstI 10, EndTrustBlock)
            )
        ),
        Execute (Var "myUtrustB")
    )
) env false list;;
print_separator();;

(* Trying to nest include blocks *)
print_string " Test_10\n";;
execWithFailure(
    Let("myUtrustB",
        Include(
            Include(
                Let("a",
                    CstI 5,
                    Prim("*", Var "a", CstI 8)
                )
            )
        ),
        Execute (Var "myUtrustB")
    )
) env false list;;
print_separator();;

(* Testing of multiplication between a tainted value and an untainted one, still no trust block involved *)
print_string " Test_11\n"
let test_TU_11 = eval(
    Let("mytrustB",
        TrustBlock(
            LetSecret("x",
                CstI 1,
                LetPublic("y",
                    CstI 3,
                    Handle("y", EndTrustBlock)
                )
            )
        ),
        Let("plainCode",
            Let("extCode",
                Include(
                    Let("a",
                        CstI 2,
                        Let("b",
                            CstI 5,
                            Prim ("*", Var "b", Var "a")
                        )
                    )
                ),
                Assign("plainCode", Execute (Var "extCode"))
            ),
            Let ("g",
                CstI 10,
                Prim ("*", Var "g", Var "plainCode")
            )
        )
    )
) env false list;;
print_eval test_TU_11;;
print_separator();;

(* Testing Assert in normal code *)
print_string " Test_12\n"
let test_TU_12 = eval(
    Let ("code",
        Let ("x",
            CstI 2,
            Prim("+", Var "x", CstI 2)
        ),
        Assert "code"
    )
) env false list;;
print_eval test_TU_12;;
print_separator();;

(* Testing Assert on Untrusted Block *)
print_string " Test_13\n";;
execWithFailure(
    Let("plugin",
        Include(
            Let("x",
                CstB false,
                Let("y",
                    CstB true,
                    Prim ("&&", Var "x", Var "y")
                )
            )
        ),
        Let("exe",
            Execute (Var "plugin"),
            Assert "exe"
        )
    )
) env false list;;
print_separator();;

(* Testing Assert on a function *)
print_string " Test_14\n";;
let test_TU_14 = eval(
    Let("x",
        CstI 1,
        Let("f",
            Fun ("y",
                Prim ("-", Assert "y", Assert "x")
            ),
            Call(Assert "f", CstI 2)
        )
    )
) env false list;;
print_eval(test_TU_14);;
print_separator();;

print_string " Test_15\n";;
let test_TU_15 = eval(
    Let("mytrustB",
        TrustBlock(
            LetSecret("x",
                CstString "abcd",
                LetPublic("CheckPassword",
                    Fun("guess",
                        Prim("=", TrustedVar ("x"), Var ("guess"))
                    ),
                    Handle ("CheckPassword", EndTrustBlock)
                )
            )
        ),
        Call(AccessTrust(Var("mytrustB"), Var("CheckPassword")), CstString "abcd")
    )
  ) env false list;;
print_eval(test_TU_15);;
print_separator();;

(* Testing AccessTrust on a variable that isnt handled *)
print_string " Test_16\n";;
execWithFailure(
    Let("mytrustB",
        TrustBlock(
            LetSecret("x", CstI 1,
                LetPublic("c", CstI 2,
                    LetPublic("y", CstI 3,
                        Handle("y", EndTrustBlock)
                    )
                )
            )
        ),
        Let("plainCode",
            Let("extCode",
                Include(Let("a", CstI 5,
                            Let("b", CstI 5,
                                Prim("*", Var("b"), Var("a"))
                               )
                           )
                       ),
                Assign("plainCode", Execute(Var("extCode")))
            ),
            AccessTrust(Var("mytrustB"), Var("c"))
        )
    )
) env false list;;
print_separator();;
