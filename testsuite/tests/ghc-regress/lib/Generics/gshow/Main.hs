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
