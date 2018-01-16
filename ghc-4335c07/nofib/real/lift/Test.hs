module Test where
import LambdaLift
import Utilities
program =
    (ELet True [("p", ELam ["a", "b"] (ELet False [("p", (EAp (EAp (EConst
    (CFun "-")) (EAp (EAp (EConst (CFun "+")) (EVar "a")) (EVar "b")))
    (EConst (CNum 20))))] (ELet False [("m", ELam ["p", "q"] (ELet False
    [("g", (EAp (EAp (EConst (CFun "+")) (EVar "a")) (EConst (CNum 6))))]
    (ELet False [("d", ELam ["pp"] (EAp (EAp (EConst (CFun "+")) (EAp (EAp
    (EConst (CFun "+")) (EVar "pp")) (EVar "q"))) (EVar "p")))] (ELet False
    [("c", ELam ["q"] (EAp (EAp (EConst (CFun "+")) (EAp (EAp (EConst (CFun
    "-")) (EVar "q")) (EVar "g"))) (EVar "a")))] (EAp (EAp (EConst (CFun
    "-")) (EAp (EAp (EConst (CFun "-")) (EAp (EVar "d") (EAp (EAp (EConst
    (CFun "+")) (EConst (CNum 1))) (EVar "a")))) (EAp (EVar "c") (EAp (EAp
    (EConst (CFun "-")) (EVar "a")) (EVar "b"))))) (EVar "p"))))))] (ELet
    True [("f", ELam ["a", "b"] (EAp (EAp (EVar "g") (EVar "a")) (EAp (EAp
    (EConst (CFun "+")) (EVar "b")) (EConst (CNum 1))))), ("g", ELam ["x",
    "y"] (EAp (EAp (EVar "f") (EVar "x")) (EAp (EAp (EConst (CFun "+"))
    (EVar "y")) (EVar "b"))))] (ELet False [("h", ELam ["a", "n"] (EAp (EAp
    (EVar "m") (EAp (EAp (EConst (CFun "+")) (EVar "n")) (EConst (CNum
    1)))) (EAp (EAp (EVar "f") (EVar "a")) (EVar "n"))))] (EAp (EAp (EVar
    "h") (EVar "a")) (EVar "p")))))))] (EAp (EAp (EVar "p") (EConst (CNum
    5))) (EConst (CNum 6))))
