{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Show.Integer
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Utilities for showing integers.
--
-----------------------------------------------------------------------------


module GHC.Show.Integer
    ( showIntAtBase
    , showInt
    , showBin
    , showHex
    , showOct
    ) where

import GHC.Base
import GHC.Num
import GHC.Real
import GHC.Show

-- | Show /non-negative/ 'Integral' numbers in base 10.
showInt :: Integral a => a -> ShowS
showInt n0 cs0
    | n0 < 0    = errorWithoutStackTrace "Numeric.showInt: can't show negative numbers"
    | otherwise = go n0 cs0
    where
    go n cs
        | n < 10    = case unsafeChr (ord '0' + fromIntegral n) of
            c@(C# _) -> c:cs
        | otherwise = case unsafeChr (ord '0' + fromIntegral r) of
            c@(C# _) -> go q (c:cs)
        where
        (q,r) = n `quotRem` 10


-- | Shows a /non-negative/ 'Integral' number using the base specified by the
-- first argument, and the character representation specified by the second.
showIntAtBase :: Integral a => a -> (Int -> Char) -> a -> ShowS
showIntAtBase base toChr n0 r0
  | base <= 1 = errorWithoutStackTrace ("Numeric.showIntAtBase: applied to unsupported base " ++ show (toInteger base))
  | n0 <  0   = errorWithoutStackTrace ("Numeric.showIntAtBase: applied to negative number " ++ show (toInteger n0))
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

-- | Show /non-negative/ 'Integral' numbers in base 8.
showOct :: Integral a => a -> ShowS
showOct = showIntAtBase 8  intToDigit

-- | Show /non-negative/ 'Integral' numbers in base 2.
showBin :: Integral a => a -> ShowS
showBin = showIntAtBase 2  intToDigit
