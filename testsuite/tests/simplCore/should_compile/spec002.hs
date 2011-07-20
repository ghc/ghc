{-# OPTIONS_GHC -O2 #-}

-- Ths one fooled the rule-matching in SpecConstr, and gave a CoreLint error

module Foo where

type Token = (Int, Int, Lex, String)

data Lex = Llbrace

laLayout :: Int -> [Int] -> [Token] -> [Token]

laLayout l (s:ss) (t1@(l1, n1, w1, c1) :
                   t2@(l2, n2, w2, c2) : ts)
   | n1 < s    = laLayout l2 (n2:s:ss) ts 
   | otherwise = laLayout l ss (t1:t2:ts)

