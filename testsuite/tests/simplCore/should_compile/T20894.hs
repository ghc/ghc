{-# LANGUAGE Haskell2010 #-}

module T20894 where

import Prelude (Int)

data LitE = IntLit !Int | StringLit
data Term = LitE LitE | Term :$ Term | S Term | VarE
data Val = LitV LitE

eval :: Term -> Val
eval (LitE l)  = LitV l
eval (S a) = eval a
eval _ = LitV StringLit

church :: Int -> Term
church 0 = VarE
church _ = S VarE

evalChurchId :: Int -> Int -> Int
evalChurchId i arg =
  case eval (S (S (church i)) :$ LitE (IntLit arg) ) of
    LitV (IntLit res) -> res
