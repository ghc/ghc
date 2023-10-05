{-# LANGUAGE GADTs #-}
module T3632 where

import Data.Char ( ord )

data T where
  MkT :: { f :: a -> Int, x :: a, wombat :: String } -> T

foo :: T -> T
foo t = t { f = ord, x = '3' }
