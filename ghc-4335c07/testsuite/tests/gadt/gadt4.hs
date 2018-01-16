{-# LANGUAGE GADTs #-}

module Main where

data Term a where
   Lit    :: Int -> Term Int  
   IsZero :: Term Int -> Term Bool
   If     :: Term Bool -> Term a -> Term a -> Term a


eval :: Term a -> a
eval (Lit n)       = n  
eval (IsZero t)    = eval t == 0
eval (If t1 t2 t3) = if eval t1 then eval t2 else eval t3

term = If (IsZero (Lit 1)) (Lit 2) (Lit 3)

main = print (eval term)