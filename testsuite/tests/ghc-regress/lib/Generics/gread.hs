{-# OPTIONS -fglasgow-exts #-}

{-

The most simple example to illustrate gread.

-}

module Main where
import Data.Generics

str1 = "(True)" -- reads fine as a Bool
str2 = "(Treu)" -- invalid constructor
str3 = "True"   -- lacks parentheses

main = print ([ gread str1,
                gread str2,
                gread str3 
              ] :: [Maybe (Bool, String)])

