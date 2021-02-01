{-# LANGUAGE MagicHash #-}
module T18561 where

import GHC.Exts
import Data.Function

x :: Int
x = I# ("" & error)
