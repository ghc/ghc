{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

module Array (
        module Ix,  -- export all of Ix for convenience
        Array, array, listArray, (!), bounds, indices, elems, assocs,
        accumArray, (//), accum, ixmap
    ) where

import Ix
import Data.Array
