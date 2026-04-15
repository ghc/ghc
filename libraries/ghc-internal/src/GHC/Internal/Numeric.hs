{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Numeric
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Odds and ends, mostly functions for reading and showing
-- 'RealFloat'-like kind of values.
--
-----------------------------------------------------------------------------

module GHC.Internal.Numeric (showIntAtBase, showHex) where

import GHC.Internal.Prim (seq)
import GHC.Internal.Types (Char, Int)
import GHC.Internal.Classes ((<), (<=))
import GHC.Internal.Err (errorWithoutStackTrace)
import GHC.Internal.Base (($), otherwise)
import GHC.Internal.List ((++))
import GHC.Internal.Real (Integral, toInteger, fromIntegral, quotRem)
import GHC.Internal.Show (ShowS, show, intToDigit)

-- ---------------------------------------------------------------------------
-- Integer printing functions

-- | Shows a /non-negative/ 'Integral' number using the base specified by the
-- first argument, and the character representation specified by the second.
showIntAtBase :: Integral a => a -> (Int -> Char) -> a -> ShowS
showIntAtBase base toChr n0 r0
  | base <= 1 = errorWithoutStackTrace ("GHC.Internal.Numeric.showIntAtBase: applied to unsupported base " ++ show (toInteger base))
  | n0 <  0   = errorWithoutStackTrace ("GHC.Internal.Numeric.showIntAtBase: applied to negative number " ++ show (toInteger n0))
  | otherwise = showIt (quotRem n0 base) r0
   where
    showIt (n,d) r = seq c $ -- stricter than necessary
      case n of
        0 -> r'
        _ -> showIt (quotRem n base) r'
     where
      c  = toChr (fromIntegral d)
      r' = c : r

-- | Show /non-negative/ 'Integral' numbers in base 16.
showHex :: Integral a => a -> ShowS
showHex = showIntAtBase 16 intToDigit
