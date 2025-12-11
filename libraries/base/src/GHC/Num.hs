{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK not-home #-}
-- don't warn that some but not all of Integer and Natural are deprecated
{-# OPTIONS_GHC -Wno-incomplete-export-warnings -Wno-duplicate-exports #-}

-- |
-- Module      :  GHC.Num
-- Copyright   :  (c) The University of Glasgow 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Num' class and the 'Integer' type.
--

module GHC.Num
   ( Num(..)
   , Integer
   , Natural
   , subtract
   , quotRemInteger
   , integerToWord
   , integerFromWord
   , integerToInt
   , integerFromInt
   , integerToNatural
   , integerFromNatural
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     Integer(IN, IP, IS)
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     Natural(NS, NB)
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerToNaturalClamp
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerToNaturalThrow
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerToWord#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerToInt#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerToWord64#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerToInt64#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerAdd
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerMul
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerSub
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerNegate
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerAbs
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerPopCount#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerQuot
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerRem
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerDiv
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerMod
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerDivMod#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerQuotRem#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerEncodeFloat#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerEncodeDouble#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerGcd
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerLcm
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerAnd
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerOr
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerXor
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerComplement
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerBit#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerTestBit#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerShiftL#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerShiftR#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerFromWord#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerFromWord64#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerFromInt64#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerBit
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerCheck
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerCheck#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerCompare
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerDecodeDouble#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerDivMod
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerEncodeDouble
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerEq
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerEq#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerFromAddr
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerFromAddr#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerFromBigNat#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerFromBigNatNeg#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerFromBigNatSign#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerFromByteArray
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerFromByteArray#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerFromInt#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerFromWordList
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerFromWordNeg#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerFromWordSign#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerGcde
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerGcde#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerGe
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerGe#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerGt
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerGt#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerIsNegative
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerIsNegative#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerIsOne
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerIsPowerOf2#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerIsZero
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerLe
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerLe#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerLog2
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerLog2#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerLogBase
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerLogBase#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerLogBaseWord
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerLogBaseWord#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerLt
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerLt#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerNe
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerNe#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerOne
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerPowMod#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerQuotRem
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerRecipMod#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerShiftL
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerShiftR
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerSignum
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerSignum#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerSizeInBase#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerSqr
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerTestBit
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerToAddr
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerToAddr#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerToBigNatClamp#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerToBigNatSign#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerToMutableByteArray
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerToMutableByteArray#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     integerZero
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalToWord#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalPopCount#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalShiftR#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalShiftL#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalAdd
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalSub
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalSubThrow
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalSubUnsafe
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalMul
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalQuotRem#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalQuot
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalRem
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalAnd
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalAndNot
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalOr
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalXor
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalTestBit#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalBit#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalGcd
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalLcm
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalLog2#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalLogBaseWord#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalLogBase#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalPowMod
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalSizeInBase#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalBit
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalCheck
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalCheck#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalClearBit
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalClearBit#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalCompare
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalComplementBit
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalComplementBit#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalEncodeDouble#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalEncodeFloat#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalEq
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalEq#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalFromAddr
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalFromAddr#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalFromBigNat#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalFromByteArray#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalFromWord
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalFromWord#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalFromWord2#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalFromWordList
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalGe
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalGe#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalGt
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalGt#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalIsOne
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalIsPowerOf2#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalIsZero
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalLe
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalLe#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalLog2
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalLogBase
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalLogBaseWord
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalLt
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalLt#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalNe
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalNe#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalNegate
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalOne
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalPopCount
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalQuotRem
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalSetBit
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalSetBit#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalShiftL
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalShiftR
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalSignum
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalSqr
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalTestBit
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalToAddr
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalToAddr#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalToBigNat#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalToMutableByteArray#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalToWord
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalToWordClamp
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalToWordClamp#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalToWordMaybe#
   , {-# DEPRECATED ["The internals of big numbers will be removed in 4.24.", "Use ghc-bignum instead."] #-}
     naturalZero
   )
where

import GHC.Internal.Num
