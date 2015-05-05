-- test the representation of literals and also explicit type annotations

module TH_repE1
where

import Language.Haskell.TH

integralExpr :: ExpQ
integralExpr = [| 42 |]

intExpr :: ExpQ
intExpr = [| 42 :: Int |]

integerExpr :: ExpQ
integerExpr = [| 42 :: Integer |]

charExpr :: ExpQ
charExpr = [| 'x' |]

stringExpr :: ExpQ
stringExpr = [| "A String" |]

fractionalExpr :: ExpQ
fractionalExpr = [| 1.2 |]

floatExpr :: ExpQ
floatExpr = [| 1.2 :: Float |]

doubleExpr :: ExpQ
doubleExpr = [| 1.2 :: Double |]
