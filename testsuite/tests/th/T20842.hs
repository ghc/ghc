{-# LANGUAGE TemplateHaskell, GADTs #-}
module Main where

import Language.Haskell.TH

main :: IO ()
main = do
  x <- [d| data MyData where { D1 :: MyData; D2 :: Bool -> MyData } |]
  putStrLn $ pprint x
