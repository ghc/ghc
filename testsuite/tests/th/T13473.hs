{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH
import T13473a

[quoter|y|] = 1

main :: IO ()
main = do
  let $(varP $ mkName "x") = 1 in print x
  print y
