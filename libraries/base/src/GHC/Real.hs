{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  GHC.Real
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The types 'Ratio' and 'Rational', and the classes 'Real', 'Fractional',
-- 'Integral', and 'RealFrac'.
--

module GHC.Real
    ( -- * Classes
      Real(..)
    , Integral(..)
    , Fractional(..)
    , RealFrac(..)

      -- * Conversion
    , fromIntegral
    , realToFrac

      -- * Formatting
    , showSigned

      -- * Predicates
    , even
    , odd

      -- * Arithmetic
    , (^)
    , (^^)
    , gcd
    , lcm

      -- * 'Ratio'
    , Ratio(..)
    , Rational
    , infinity
    , notANumber

      -- * 'Enum' helpers
    , numericEnumFrom
    , numericEnumFromThen
    , numericEnumFromTo
    , numericEnumFromThenTo
    , integralEnumFrom
    , integralEnumFromThen
    , integralEnumFromTo
    , integralEnumFromThenTo

      -- ** Construction
    , (%)

      -- ** Projection
    , numerator
    , denominator

      -- ** Operations
    , reduce

      -- * Internal
    , ratioPrec
    , ratioPrec1
    , divZeroError
    , ratioZeroDenominatorError
    , overflowError
    , underflowError
    , mkRationalBase2
    , mkRationalBase10
    , FractionalExponentBase(..)
    , (^%^)
    , (^^%^^)
    , mkRationalWithExponentBase
    , powImpl
    , powImplAcc
    ) where

import GHC.Internal.Real
