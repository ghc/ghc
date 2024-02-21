{-# LANGUAGE Safe #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Compatibility module for pre ghc-bignum code.

module GHC.Natural
    (Natural(NatS#, NatJ#),
     BigNat(..),
     mkNatural,
     isValidNatural,
     -- *  Arithmetic
     plusNatural,
     minusNatural,
     minusNaturalMaybe,
     timesNatural,
     negateNatural,
     signumNatural,
     quotRemNatural,
     quotNatural,
     remNatural,
     gcdNatural,
     lcmNatural,
     -- *  Bits
     andNatural,
     orNatural,
     xorNatural,
     bitNatural,
     testBitNatural,
     popCountNatural,
     shiftLNatural,
     shiftRNatural,
     -- *  Conversions
     naturalToInteger,
     naturalToWord,
     naturalToWordMaybe,
     wordToNatural,
     wordToNatural#,
     naturalFromInteger,
     -- *  Modular arithmetic
     powModNatural
     ) where

import GHC.Internal.Natural
