module T22985b where

type Phase n = n

addExpr :: Num a => Phase a -> a
addExpr x = let t = asTypeOf x 0 in t
