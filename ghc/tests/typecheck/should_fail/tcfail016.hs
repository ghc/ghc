module ShouldFail where

type AnnExpr a = (a,Expr a)

data Expr a = Var [Char]
              | App (AnnExpr a) (AnnExpr a)

g (Var name) = [name]
g (App e1 e2) = (g e1)++(g e2)
