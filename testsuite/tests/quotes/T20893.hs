-- #20893

{-# LANGUAGE LambdaCase #-}

module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Ppr

main = do
  runQ t1 >>= p
  runQ t2 >>= p
  runQ t3 >>= p
  runQ t4 >>= p

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

t3 = [d|
      main = do
         let color = "red"
         let id = 1
         print_color (color, id)
       where print_color (c, i) = putStrLn (c ++ " is " ++ (show i))
      |]

t4 = [d|
      main = do
         let colors = ["red", "green", "blue"]
         let ids = map (\case
                           "red" -> 0
                           "green" -> 1
                           "blue" -> 2) colors
         putStrLn (show ids)
      |]

p = putStrLn . pprint
