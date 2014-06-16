{-# LANGUAGE QuasiQuotes, ViewPatterns #-}

module Main where

import A

main = do
  case 1 of
    [foo|x|] -> print x
  case 1 of
    [bar|<!anything~|] -> print fixed_var
