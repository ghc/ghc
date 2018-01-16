{-# LANGUAGE ViewPatterns, QuasiQuotes #-}

module Main where

import Language.Haskell.TH

main = do
  p <- runQ [p|(id -> x)|]
  print p
  putStrLn (pprint p)
