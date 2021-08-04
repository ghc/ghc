{-# OPTIONS -fglasgow-exts #-}

module GShow2 (tests) where

{-

This test exercices GENERIC show for the infamous company datatypes. The
output of the program should be some representation of the infamous
"genCom" company.

-}

import Test.Tasty.HUnit

import Data.Generics
import CompanyDatatypes

tests = gshow genCom @=? output

{-

Here is another exercise:
The following function gshow' is a completely generic variation on gshow.
It would print strings as follows:

*Main> gshow' "abc"
"((:) ('a') ((:) ('b') ((:) ('c') ([]))))"

The original gshow does a better job because it is customised for strings:

*Main> gshow "foo"
"\"foo\""

In fact, this is what Haskell's normal show would also do:

*Main> show "foo"
"\"foo\""

-}

gshow' :: Data a => a -> String
gshow' t =     "("
            ++ showConstr (toConstr t)
            ++ concat (gmapQ ((++) " " . gshow') t)
            ++ ")"

output = "(C ((:) (D \"Research\" (E (P \"Laemmel\" \"Amsterdam\") (S (8000.0))) ((:) (PU (E (P \"Joost\" \"Amsterdam\") (S (1000.0)))) ((:) (PU (E (P \"Marlow\" \"Cambridge\") (S (2000.0)))) ([])))) ((:) (D \"Strategy\" (E (P \"Blair\" \"London\") (S (100000.0))) ([])) ([]))))"
