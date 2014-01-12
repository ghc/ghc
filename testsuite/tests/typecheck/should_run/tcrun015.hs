-- !!! Newtypes

-- A variation of tc014 that Sigbjorn said failed

module Main where

data Expr a b = One a | Many [b]
newtype Pat a   = InP (Expr a (Pat a), Int)
newtype PExpr a = InPE (Expr a (PExpr a), Int)

plus1 x@(InPE (_, loc)) = InPE (Many [x], loc)
one x l = InPE (One (plus1 x), l)

outP (InP x) = x

getPatNames p
  = case outP p of
      (One n, _)     -> [n]
      (Many ps, _) -> concatMap getPatNames ps

main = print (take 10 (map getPatNames (repeat (InP (One "n", 1)))))
