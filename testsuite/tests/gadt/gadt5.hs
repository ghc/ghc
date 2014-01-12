{-# LANGUAGE GADTs #-}

module Main where

data Term a where
   Lit    :: Int -> Term Int  
   IsZero :: Term Int -> Term Bool
   If     :: Term Bool -> Term a -> Term a -> Term a
   Pr  	  :: Term a -> Term b -> Term (a, b)
   Fst 	  :: Term (a, b) -> Term a
   Snd 	  :: Term (a, b) -> Term b

eval :: Term v -> v
eval (Lit n)       = n  
eval (IsZero t)    = eval t == 0
eval (If t1 t2 t3) = if eval t1 then eval t2 else eval t3
eval (Pr t1 t2)    = (eval t1, eval t2)
eval (Fst t)	   = case (eval t) of { (a,b) -> a }
eval (Snd t)	   = case (eval t) of { (a,b) -> b }

term = If (IsZero (Lit 1)) (Pr (Lit 2) (Lit 3)) (Pr (Lit 3) (Lit 4))

main = print (eval term)