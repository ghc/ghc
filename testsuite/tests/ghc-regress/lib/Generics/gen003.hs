{-# OPTIONS -fglasgow-exts #-}

-- This test exercices GENERIC read, show, and eq.

module Main where
import Data.Generics
import CompanyDatatypes

main = print (maybe False
                    (geq genCom . fst)
                    (gread (gshow genCom)))
