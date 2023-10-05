{-# LANGUAGE PatternSynonyms #-}
module Main where

pattern ReqNoProv :: Show a => a -> Maybe a
pattern ReqNoProv{j} = Just j

data A = A deriving Show

p1 = Just True

-- The record updates in 'p6' is desugared to the following, as per #18802:
-- p6 = case p1 of ReqNoProv x -> ReqNoProv A
-- (Details of this desugaring can be found in Note [Record Updates] in Tc.Gen.Expr)

p6 = p1 {j = A}

main = print p6
