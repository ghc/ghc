module T22629d_Lib where

import GHC.Exts
import GHC.Int
import Data.List.NonEmpty as NE
import Data.Foldable as F

{-# INLINABLE getNumbers #-}
{-# NOINLINE getNumbers #-}
getNumbers :: Num a => NonEmpty a
getNumbers = 42 :| (F.toList getNumbers)
