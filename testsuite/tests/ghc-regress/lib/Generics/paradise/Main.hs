{-# OPTIONS -fglasgow-exts #-}

{-

This test runs the infamous PARADISE benchmark,
which is the HELLO WORLD example of generic programming,
i.e., the "increase salary" function is applied to
a typical company just as shown in the boilerplate paper.

-}

module Main where
import Data.Generics
import CompanyDatatypes

-- Increase salary by percentage
increase :: Float -> Company -> Company
increase k = everywhere (mkT (incS k))

-- "interesting" code for increase
incS :: Float -> Salary -> Salary
incS k (S s) = S (s * (1+k))

main = print $ increase 0.1 genCom
