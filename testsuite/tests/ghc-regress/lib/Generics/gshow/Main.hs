{-# OPTIONS -fglasgow-exts #-}

{-

This test exercices GENERIC show for the infamous company datatypes. The
output of the program should be some representation of the infamous
"genCom" company.

-}

module Main where
import Data.Generics
import CompanyDatatypes

main = print $ gshow genCom


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
