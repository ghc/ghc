-- test the representation of literals and also explicit type annotations

module TH_repE1
where

import Language.Haskell.THSyntax

intExpr :: Expr
intExpr = [| 42 :: Int |]

integerExpr :: Expr
integerExpr = [| 42 :: Integer |]

charExpr :: Expr
charExpr = [| 'x' |]

stringExpr :: Expr
stringExpr = [| "A String" |]

floatExpr :: Expr
floatExpr = [| 1.2 :: Float |]

doubleExpr :: Expr
doubleExpr = [| 1.2 :: Double |]
