{-# LANGUAGE TemplateHaskell #-}

module T5434a where

import Language.Haskell.TH

genShadow1 :: Q [Dec]
genShadow1 = 
  [d| x :: Char
      x = 'x'
    |]

genShadow2 :: Q [Dec]
genShadow2 =
  [d| z :: Char
      z = succ x
        where x = 'y'
    |]     
