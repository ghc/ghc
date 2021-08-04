{-# OPTIONS -fglasgow-exts #-}

module Paradise (tests) where

{-

This test runs the infamous PARADISE benchmark,
which is the HELLO WORLD example of generic programming,
i.e., the "increase salary" function is applied to
a typical company just as shown in the boilerplate paper.

-}

import Test.Tasty.HUnit

import Data.Generics
import CompanyDatatypes

-- Increase salary by percentage
increase :: Double -> Company -> Company
increase k = everywhere (mkT (incS k))

-- "interesting" code for increase
incS :: Double -> Salary -> Salary
incS k (S s) = S (s * (1+k))

tests = increase 0.125 genCom @=? output

output = C [D "Research" (E (P "Laemmel" "Amsterdam") (S 9000)) [PU (E (P "Joost" "Amsterdam") (S 1125)),PU (E (P "Marlow" "Cambridge") (S 2250))],D "Strategy" (E (P "Blair" "London") (S 112500)) []]
