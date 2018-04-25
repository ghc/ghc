-- !!! Newtypes

-- This one made ghc 5.01 (after newtype squashing) fall over
-- by generating Core code that contained a pattern match on 
-- the InPE data constructor (which doesn't exist)

module Main where


data Expr e = One e | Many [e]
newtype PExpr a = InPE (Expr (PExpr a), Int)

one :: Int -> PExpr e -> PExpr e
one l x = InPE (One (plus1 x), l)

plus1 :: PExpr a -> PExpr a
plus1 x@(InPE (_, loc)) = InPE (Many [plus1 x], loc)

get :: PExpr e -> Int
get (InPE (_,l)) = l

main = print (get (plus1 (InPE (Many [], 0))))
