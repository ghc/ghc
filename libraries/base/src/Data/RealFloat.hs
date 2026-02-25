{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ViewPatterns #-}

-- |
--
-- Module      :  Data.RealFloat
-- Copyright   :  (c) The University of Glasgow 2026
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--

module Data.RealFloat (
  RealFloat (..),

  -- * Infinity + NaN
  pattern Infinity,
  pattern NegInfinity,
  pattern NaN,
) where

import Data.Bool
import GHC.Internal.Data.Ord
import GHC.Internal.Float
import GHC.Internal.Real

pattern Infinity :: (RealFloat a) => a
pattern Infinity <- ((\x -> isInfinite x && x > 0) -> True) where Infinity = 1/0

-- | Negative infinity
--
-- Provided for convenience. Could also use the following instead:
--   * Pattern matching: @(negate -> Infinity)@
--   * Expressions: @-Infinity@
pattern NegInfinity :: (RealFloat a) => a
pattern NegInfinity <- ((\x -> isInfinite x && x < 0) -> True) where NegInfinity = -1/0

-- | A pattern synonym for NaN values.
--
-- Note: Per IEEE 754, NaN is never equal to itself, thus these two snippets
-- have different behavior:
--
-- @
-- -- foo1 NaN == "a"
-- foo1 NaN = "a"
-- foo1 _ = "b"
--
-- -- foo2 NaN == "b"
-- foo2 x = if x == NaN then "a" else "b"
-- @
pattern NaN :: (RealFloat a) => a
pattern NaN <- (isNaN -> True) where NaN = 0/0
