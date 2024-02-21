{-# LANGUAGE Safe #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.Float
-- Copyright   :  (c) The University of Glasgow 1994-2002
--                Portions obtained from hbc (c) Lennart Augusstson
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The types 'Float' and 'Double', the classes 'Floating' and 'RealFloat' and
-- casting between Word32 and Float and Word64 and Double.
--

module GHC.Float
    (-- *  Classes
     Floating(..),
     RealFloat(..),
     -- *  'Float'
     Float(..),
     Float#,
     -- **  Conversion
     float2Int,
     int2Float,
     word2Float,
     integerToFloat#,
     naturalToFloat#,
     rationalToFloat,
     castWord32ToFloat,
     castFloatToWord32,
     castWord32ToFloat#,
     castFloatToWord32#,
     float2Double,
     -- **  Operations
     floorFloat,
     ceilingFloat,
     truncateFloat,
     roundFloat,
     properFractionFloat,
     -- **  Predicate
     isFloatDenormalized,
     isFloatFinite,
     isFloatInfinite,
     isFloatNaN,
     isFloatNegativeZero,
     -- **  Comparison
     gtFloat,
     geFloat,
     leFloat,
     ltFloat,
     -- **  Arithmetic
     plusFloat,
     minusFloat,
     timesFloat,
     divideFloat,
     negateFloat,
     expFloat,
     expm1Float,
     logFloat,
     log1pFloat,
     sqrtFloat,
     fabsFloat,
     sinFloat,
     cosFloat,
     tanFloat,
     asinFloat,
     acosFloat,
     atanFloat,
     sinhFloat,
     coshFloat,
     tanhFloat,
     asinhFloat,
     acoshFloat,
     atanhFloat,
     -- *  'Double'
     Double(..),
     Double#,
     -- **  Conversion
     double2Int,
     int2Double,
     word2Double,
     integerToDouble#,
     naturalToDouble#,
     rationalToDouble,
     castWord64ToDouble,
     castDoubleToWord64,
     castWord64ToDouble#,
     castDoubleToWord64#,
     double2Float,
     -- **  Operations
     floorDouble,
     ceilingDouble,
     truncateDouble,
     roundDouble,
     properFractionDouble,
     -- **  Predicate
     isDoubleDenormalized,
     isDoubleFinite,
     isDoubleInfinite,
     isDoubleNaN,
     isDoubleNegativeZero,
     -- **  Comparison
     gtDouble,
     geDouble,
     leDouble,
     ltDouble,
     -- **  Arithmetic
     plusDouble,
     minusDouble,
     timesDouble,
     divideDouble,
     negateDouble,
     expDouble,
     expm1Double,
     logDouble,
     log1pDouble,
     sqrtDouble,
     fabsDouble,
     sinDouble,
     cosDouble,
     tanDouble,
     asinDouble,
     acosDouble,
     atanDouble,
     sinhDouble,
     coshDouble,
     tanhDouble,
     asinhDouble,
     acoshDouble,
     atanhDouble,
     -- *  Formatting
     showFloat,
     FFFormat(..),
     formatRealFloat,
     formatRealFloatAlt,
     showSignedFloat,
     -- *  Operations
     log1mexpOrd,
     roundTo,
     floatToDigits,
     integerToBinaryFloat',
     fromRat,
     fromRat',
     roundingMode#,
     -- *  Monomorphic equality operators
     -- |  See GHC.Classes#matching_overloaded_methods_in_rules
     eqFloat,
     eqDouble,
     -- *  Internal
     -- |  These may vanish in a future release
     clamp,
     expt,
     expts,
     expts10,
     fromRat'',
     maxExpt,
     maxExpt10,
     minExpt,
     powerDouble,
     powerFloat,
     stgDoubleToWord64,
     stgFloatToWord32,
     stgWord64ToDouble,
     stgWord32ToFloat
     ) where

import GHC.Internal.Float
