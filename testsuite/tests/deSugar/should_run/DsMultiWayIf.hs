{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.List (isSuffixOf)
import Control.Exception

errMsg = "Non-exhaustive guards in multi-way if\n"
table  = [(1, "one"), (100, "hundred")]

f t x = if | l <- length t, l > 2, l < 5 -> "length is 3 or 4"
           | Just y <- lookup x t -> y
           | False -> "impossible"
           | null t -> "empty"

main = do
  print $ [ f table 1   == "one"
          , f table 100 == "hundred"
          , f [] 1      == "empty"
          , f [undefined, undefined, undefined] (undefined :: Bool) ==
              "length is 3 or 4"
          , f ((0, "zero") : table) 100 == "length is 3 or 4"
          ]
  r <- try $ evaluate $ f table 99
  print $ case r of
    Left (PatternMatchFail s) | errMsg `isSuffixOf` s -> True
    _                                                 -> False

