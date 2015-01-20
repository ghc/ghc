{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Ratio (
        Ratio, Rational, (%), numerator, denominator, approxRational
    ) where

import Data.Ratio
