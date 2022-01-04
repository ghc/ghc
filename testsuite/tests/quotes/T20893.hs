-- #20893

module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Ppr

main = do
  runQ t1 >>= p
  runQ t2 >>= p

t1 = [d| main = do { case 0 of { 0 -> 1 }; putStrLn "pass" } |]

t2 = [d|
      main = do
         let day = "mon"
         let num = case day of
               "mon" -> 0
               "tue" -> 1
               "wed" -> 3
               "thu" -> 4
               "fri" -> 5
               "sat" -> 6
               "sun" -> 7
               _ -> 8
         putStrLn (show day) ++ " is " (show num)
      |]

p = putStrLn . pprint
