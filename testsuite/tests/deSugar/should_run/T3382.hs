{-# OPTIONS_GHC -XOverloadedStrings #-}
module Main where

import Data.String

instance IsString Int where
         fromString x = 1337

f :: Int -> String
f "hello" = "correct"
f _       = "false"

main = do print $ f 1337
          print $ f 1338
