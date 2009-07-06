{-# OPTIONS_GHC -XFlexibleContexts -fwarn-unused-imports #-}

module ArrayBoundedU
   ( T
   , create
   , at
   ) where

import Data.Ix
import qualified Data.Array.Unboxed as Array
import Data.Array.Base (unsafeAt)

newtype T i e = T (Array.UArray i e)

create :: (Ix i, Bounded i, Array.IArray Array.UArray e) => [(i,e)] -> T i e
create ies = T (Array.array (minBound, maxBound) ies)

at :: (Ix i, Bounded i, Array.IArray Array.UArray e) => T i e -> i -> e
at (T a) i = unsafeAt a (index (minBound, maxBound) i)
