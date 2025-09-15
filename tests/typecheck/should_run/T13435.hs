{-# Language FlexibleInstances, TypeFamilies,
             DataKinds, PolyKinds, MagicHash #-}

module Main where

import Data.Kind
import GHC.Exts

class Shw (a :: TYPE rep) where
  shw :: a -> String

instance Int# ~ a => Shw (a :: TYPE IntRep) where
  shw a = "I#" ++ show (I# a)

main = putStrLn (shw 3#)
