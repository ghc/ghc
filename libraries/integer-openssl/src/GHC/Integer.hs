{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Integer
  ( -- * Constructors and conversion
    Integer
  , mkInteger
  , smallInteger
  , wordToInteger
  , integerToWord
  , integerToInt
  , floatFromInteger
  , doubleFromInteger
  , encodeFloatInteger
  , encodeDoubleInteger
  , decodeDoubleInteger
  , hashInteger
  -- * Arithmetic operations
  , plusInteger
  , minusInteger
  , negateInteger
  , timesInteger
  , absInteger
  , signumInteger
  , quotRemInteger
  , quotInteger
  , remInteger
  , divModInteger
  , divInteger
  , modInteger
  , gcdInteger
  , lcmInteger
  -- * Bit operations
  , complementInteger
  , andInteger
  , orInteger
  , xorInteger
  , shiftLInteger
  , shiftRInteger
  , popCountInteger
  , bitInteger
  , testBitInteger
  -- * Comparison
  , compareInteger
  , eqInteger
  , neqInteger
  , geInteger
  , gtInteger
  , leInteger
  , ltInteger
  ) where

import GHC.Integer.Type
