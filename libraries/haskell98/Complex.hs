{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Complex (
        Complex((:+)), realPart, imagPart, conjugate,
        mkPolar, cis, polar, magnitude, phase
    ) where

import Data.Complex
