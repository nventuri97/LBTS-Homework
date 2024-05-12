open Homework_1.Interpreter

let execWithFailure test env list=
  try
    let result = eval test env list in
      (* Convertire il risultato in value per usare print_eval *)
    result
  with Failure msg -> 
    String ("Error: " ^ msg)
  
let env = [];;
let list = ([],[],[]);; 

let test_let_and_prim = execWithFailure (
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
