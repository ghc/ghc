----------------------------------------------------------------
-- Henk
-- Copyright 2000, Jan-Willem Roorda
----------------------------------------------------------------
module Main where

import Text.ParserCombinators.Parsec

import HenkAS
import HenkParser


welcome = "__   __ ______ __   __ ____   __________________________________________\n"++
          "||   || ||  || ||\\  || ||//   Henk 2000: Based on Pure Type Systems     \n"++
          "||___|| ||_|   ||\\\\ || ||\\\\                                             \n"++
          "||---|| ||-|__ || \\\\||        WWW http://www.students.cs.uu.nl/~jwroorda\n"++
          "||   || ||__||                Report bugs to: jwroorda@math.uu.nl       \n"++
          "||   || Version: Jan 2000     __________________________________________\n\n"



test fname     
        = do{ putStr welcome          
            ; result <- parseFromFile program (root ++ fname ++ ".h")
            ; case result of              
                Left err -> do{ putStr "parse error at: "
                              ; print err
                              }
                Right x  -> print x          
            }
        where
          root = ""


main = test "test"

 
