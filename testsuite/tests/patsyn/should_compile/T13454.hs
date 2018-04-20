{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module T13454 where

pattern MkOp :: Op -> Exp -> Exp -> Exp
pattern MkOp {(·), a, b} <- (splitOp -> Just ((·), a, b))
  where MkOp (·) a b      = a · b

data Exp = Val Int | Add Exp Exp | Mul Exp Exp deriving Show

type Op = Exp -> Exp -> Exp

splitOp :: Exp -> Maybe (Op, Exp, Exp)
splitOp (Add a b) = Just (Add, a, b)
splitOp (Mul a b) = Just (Mul, a, b)
splitOp _         = Nothing
