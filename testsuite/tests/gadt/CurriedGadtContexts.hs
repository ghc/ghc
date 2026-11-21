{-# LANGUAGE GADTs #-}

module Main where

import CurriedGadtContextsProvider

observe :: Packed -> (Bool, String)
observe (Curried a b) = (a == b, show b)
observe (Plain a b) = (a == b, show b)

main :: IO ()
main = do
  print (observe (Curried False True))
  print (observe (Plain 'x' 'x'))
