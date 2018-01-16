{-# OPTIONS_GHC -XTemplateHaskell #-}

-- Trac #2017

module ShouldCompile where

 import Language.Haskell.TH

 $(do e <- [d| f a b
                 | a == b = a
                 | otherwise = b |]
      return e)

