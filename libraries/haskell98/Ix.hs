{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Ix (
        Ix(range, index, inRange), rangeSize
    ) where

import Data.Ix
