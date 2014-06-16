{-# OPTIONS_GHC -XTypeOperators #-}

-- Test fixity of type operators
-- Trac #2205

module ShouldCompile where

 infixr 0 :->
 data a :-> b = P a b

 fst3:: (a :-> (b :-> c)) -> a
 fst3 (P a (P b c)) = a


 fst3':: (a :-> b :-> c) -> a
 fst3' (P a (P b c)) = a

