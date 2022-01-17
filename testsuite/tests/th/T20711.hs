{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH

main :: IO ()
main = do
  x <- [d| f (-1) = () |]
  putStrLn $ pprint x
  y <- [d| f (-10) = () |]
  putStrLn $ pprint y
