{-# LANGUAGE GADTs #-}
module DmdAnalGADTs where

-- This tests the effect of different types in branches of a case

data D a where
    A :: D Int
    B :: D (Int -> Int)

hasCPR :: Int
hasCPR = 1

hasStrSig :: Int -> Int
hasStrSig x = x

diverges :: Int
diverges = diverges

-- The result should not have a CPR property
-- Becuase we are lub’ing "m" and "<S,U>m" in the case expression.
f :: D x -> x
f x = case x of
    A -> hasCPR
    B -> hasStrSig

-- This should have the CPR property
f' :: D Int -> Int
f' x = case x of
    A -> hasCPR

-- The result should not be diverging, because one branch is terminating.
-- It should also put a strict, but not hyperstrict demand on x
g :: D x -> x
g x = case x of
    A -> diverges
    B -> \_ -> diverges


