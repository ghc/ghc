{-# OPTIONS -fglasgow-exts #-}

module Strings (tests) where

{-

This test exercices GENERIC read, show, and eq for the company
datatypes which we use a lot. The output of the program should be
"True" which means that "gread" reads what "gshow" shows while the
read term is equal to the original term in terms of "geq".

-}

import Test.Tasty.HUnit

import Data.Generics
import CompanyDatatypes

tests = (case gread (gshow genCom) of
           [(x,_)] -> geq genCom x
           _ -> False) @=? True
