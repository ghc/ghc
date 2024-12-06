{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | External BigNat backend that directly call FFI operations.
--
-- This backend can be useful for specific compilers such as GHCJS or Asterius
-- that replace bignat foreign calls with calls to the native platform bignat
-- library (e.g. JavaScript's BigInt). You can also link an extra object
-- providing the implementation.
module GHC.Internal.Bignum.Backend.FFI where

import GHC.Prim
import GHC.Types
import GHC.Internal.Bignum.WordArray
import GHC.Internal.Bignum.Primitives
import qualified GHC.Internal.Bignum.Backend.Native as Native
import {-# SOURCE #-} GHC.Internal.Bignum.Natural
import {-# SOURCE #-} GHC.Internal.Bignum.Integer

-- See W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base
-- (This module uses the empty tuple () and string literals.)
import GHC.Tuple ()
import GHC.CString ()

default ()

-- | ghc-bignum backend name
backendName :: [Char]
backendName = "ffi"

-- | Compare two non-zero BigNat of the same length
--
-- Return:
--     < 0 ==> LT
--    == 0 ==> EQ
--     > 0 ==> GT
bignat_compare
   :: WordArray#
   -> WordArray#
   -> Int#
bignat_compare = ghc_bignat_compare

foreign import ccall unsafe ghc_bignat_compare
   :: WordArray#
   -> WordArray#
   -> Int#

-- | Add two non-zero BigNat
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: max (size a, size b) + 1
--
-- The potential 0 most-significant Word (i.e. the potential carry) will be
-- removed by the caller if it is not already done by the backend.
bignat_add
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_add mwa wa wb s
   = ioVoid (ghc_bignat_add mwa wa wb) s

foreign import ccall unsafe ghc_bignat_add
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> WordArray#
   -> IO ()

-- | Add a non-zero BigNat and a non-zero Word#
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: size a + 1
--
-- The potential 0 most-significant Word (i.e. the potential carry) will be
-- removed by the caller if it is not already done by the backend.
bignat_add_word
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
bignat_add_word mwa wa b s =
   ioVoid (ghc_bignat_add_word mwa wa b) s

foreign import ccall unsafe ghc_bignat_add_word
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> Word#
   -> IO ()

-- | Multiply a non-zero BigNat and a non-zero Word#
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: size a + 1
--
-- The potential 0 most-significant Word (i.e. the potential carry) will be
-- removed by the caller if it is not already done by the backend.
bignat_mul_word
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
bignat_mul_word mwa wa b s =
   ioVoid (ghc_bignat_mul_word mwa wa b) s

foreign import ccall unsafe ghc_bignat_mul_word
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> Word#
   -> IO ()

-- | Sub two non-zero BigNat
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: size a
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
--
-- Return False# to indicate underflow.
bignat_sub
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> (# State# RealWorld, Bool# #)
bignat_sub mwa wa wb s = ioBool (ghc_bignat_sub mwa wa wb) s

foreign import ccall unsafe ghc_bignat_sub
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> WordArray#
   -> IO Bool

-- | Sub a non-zero word from a non-zero BigNat
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: size a
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
--
-- Return False# to indicate underflow.
bignat_sub_word
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> (# State# RealWorld, Bool# #)
bignat_sub_word mwa wa b s = ioBool (ghc_bignat_sub_word mwa wa b) s

foreign import ccall unsafe ghc_bignat_sub_word
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> Word#
   -> IO Bool

-- | Multiply two non-zero BigNat
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: size a+size b
--
-- The potential 0 most-significant Word (i.e. the potential carry) will be
-- removed by the caller if it is not already done by the backend.
bignat_mul
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_mul mwa wa wb s = ioVoid (ghc_bignat_mul mwa wa wb) s

foreign import ccall unsafe ghc_bignat_mul
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> WordArray#
   -> IO ()

-- | PopCount of a non-zero BigNat
bignat_popcount :: WordArray# -> Word#
bignat_popcount = ghc_bignat_popcount

foreign import ccall unsafe ghc_bignat_popcount
   :: WordArray#
   -> Word#

-- | Left-shift a non-zero BigNat by a non-zero amount of bits
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: size a + required new limbs
--
-- The potential 0 most-significant Word (i.e. the potential carry) will be
-- removed by the caller if it is not already done by the backend.
bignat_shiftl
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
bignat_shiftl mwa wa n s = ioVoid (ghc_bignat_shiftl mwa wa n) s

foreign import ccall unsafe ghc_bignat_shiftl
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> IO ()

-- | Right-shift a non-zero BigNat by a non-zero amount of bits
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: required limbs
--
-- The potential 0 most-significant Word (i.e. the potential carry) will be
-- removed by the caller if it is not already done by the backend.
bignat_shiftr
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
bignat_shiftr mwa wa n s = ioVoid (ghc_bignat_shiftr mwa wa n) s

foreign import ccall unsafe ghc_bignat_shiftr
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> IO ()

-- | Right-shift a non-zero BigNat by a non-zero amount of bits by first
-- converting it into its two's complement representation and then again after
-- the arithmetic shift.
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: required limbs
--
-- The potential 0 most-significant Words (i.e. the potential carry) will be
-- removed by the caller if it is not already done by the backend.
bignat_shiftr_neg
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
bignat_shiftr_neg mwa wa n s = ioVoid (ghc_bignat_shiftr_neg mwa wa n) s

foreign import ccall unsafe ghc_bignat_shiftr_neg
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> IO ()


-- | OR two non-zero BigNat
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: max (size a, size b)
bignat_or
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
{-# INLINE bignat_or #-}
bignat_or mwa wa wb s = ioVoid (ghc_bignat_or mwa wa wb) s

foreign import ccall unsafe ghc_bignat_or
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> IO ()

-- | XOR two non-zero BigNat
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: max (size a, size b)
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
bignat_xor
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
{-# INLINE bignat_xor #-}
bignat_xor mwa wa wb s = ioVoid (ghc_bignat_xor mwa wa wb) s

foreign import ccall unsafe ghc_bignat_xor
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> IO ()

-- | AND two non-zero BigNat
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: min (size a, size b)
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
bignat_and
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
{-# INLINE bignat_and #-}
bignat_and mwa wa wb s = ioVoid (ghc_bignat_and mwa wa wb) s

foreign import ccall unsafe ghc_bignat_and
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> IO ()

-- | ANDNOT two non-zero BigNat
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: size a
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
bignat_and_not
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
{-# INLINE bignat_and_not #-}
bignat_and_not mwa wa wb s = ioVoid (ghc_bignat_and_not mwa wa wb) s

foreign import ccall unsafe ghc_bignat_and_not
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> IO ()

-- | QuotRem of two non-zero BigNat
--
-- Result quotient and remainder are to be stored in the MutableWordArray#.
-- The first one (quotient) has size: size(A)-size(B)+1
-- The second one (remainder) has size: size(b)
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
bignat_quotrem
   :: MutableWordArray# RealWorld -- ^ Quotient
   -> MutableWordArray# RealWorld -- ^ Remainder
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_quotrem mwq mwr wa wb s =
   ioVoid (ghc_bignat_quotrem mwq mwr wa wb) s

foreign import ccall unsafe ghc_bignat_quotrem
   :: MutableWordArray# RealWorld
   -> MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> IO ()

-- | Quotient of two non-zero BigNat
--
-- Result quotient is to be stored in the MutableWordArray#.
-- The latter has size: size(A)-size(B)+1
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
bignat_quot
   :: MutableWordArray# RealWorld -- ^ Quotient
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_quot mwq wa wb s =
   ioVoid (ghc_bignat_quot mwq wa wb) s

foreign import ccall unsafe ghc_bignat_quot
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> IO ()

-- | Remainder of two non-zero BigNat
--
-- Result remainder is to be stored in the MutableWordArray#.
-- The latter has size: size(B)
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
bignat_rem
   :: MutableWordArray# RealWorld -- ^ Quotient
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_rem mwr wa wb s =
   ioVoid (ghc_bignat_rem mwr wa wb) s

foreign import ccall unsafe ghc_bignat_rem
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> IO ()

-- | QuotRem of a non-zero BigNat and a non-zero Word
--
-- Result quotient is to be stored in the MutableWordArray#.
-- The latter has size: size(A)
--
-- The remainder is returned.
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
bignat_quotrem_word
   :: MutableWordArray# RealWorld -- ^ Quotient
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> (# State# RealWorld, Word# #)
bignat_quotrem_word mwq wa b s =
   ioWord# (ghc_bignat_quotrem_word mwq wa b) s

foreign import ccall unsafe ghc_bignat_quotrem_word
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> IO Word

-- | Quot of a non-zero BigNat and a non-zero Word
--
-- Result quotient is to be stored in the MutableWordArray#.
-- The latter has size: size(A)
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
bignat_quot_word
   :: MutableWordArray# RealWorld -- ^ Quotient
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
bignat_quot_word mwq wa b s =
   ioVoid (ghc_bignat_quot_word mwq wa b) s

foreign import ccall unsafe ghc_bignat_quot_word
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> IO ()

-- | Remainder of a non-zero BigNat and a non-zero Word
--
-- The remainder is returned.
bignat_rem_word
   :: WordArray#
   -> Word#
   -> Word#
bignat_rem_word = ghc_bignat_rem_word

foreign import ccall unsafe ghc_bignat_rem_word
   :: WordArray#
   -> Word#
   -> Word#


-- | Greatest common divisor (GCD) of two non-zero and non-one BigNat
--
-- Result GCD is to be stored in the MutableWordArray#.
-- The latter has size: size(B)
-- The first WordArray# is greater than the second WordArray#.
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
bignat_gcd
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_gcd mwr wa wb s =
   ioVoid (ghc_bignat_gcd mwr wa wb) s

foreign import ccall unsafe ghc_bignat_gcd
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> IO ()

-- | Greatest common divisor (GCD) of a non-zero/non-one BigNat and a
-- non-zero/non-one Word#
--
-- Result GCD is returned
bignat_gcd_word
   :: WordArray#
   -> Word#
   -> Word#
bignat_gcd_word = ghc_bignat_gcd_word

foreign import ccall unsafe ghc_bignat_gcd_word
   :: WordArray#
   -> Word#
   -> Word#

-- | Greatest common divisor (GCD) of two Word#
--
-- Result GCD is returned
bignat_gcd_word_word
   :: Word#
   -> Word#
   -> Word#
bignat_gcd_word_word = ghc_bignat_gcd_word_word

foreign import ccall unsafe ghc_bignat_gcd_word_word
   :: Word#
   -> Word#
   -> Word#

-- | Encode (# BigNat mantissa, Int# exponent #) into a Double#
bignat_encode_double :: WordArray# -> Int# -> Double#
bignat_encode_double = ghc_bignat_encode_double

foreign import ccall unsafe ghc_bignat_encode_double
   :: WordArray#
   -> Int#
   -> Double#

-- | \"@'bignat_powmod_word' /b/ /e/ /m/@\" computes base @/b/@ raised to
-- exponent @/e/@ modulo @/m/@.
--
-- b > 1
-- e > 0
-- m > 1
bignat_powmod_word :: WordArray# -> WordArray# -> Word# -> Word#
bignat_powmod_word = ghc_bignat_powmod_word

foreign import ccall unsafe ghc_bignat_powmod_word
   :: WordArray# -> WordArray# -> Word# -> Word#

-- | \"@'bignat_powmod' r /b/ /e/ /m/@\" computes base @/b/@ raised to
-- exponent @/e/@ modulo @/m/@.
--
-- b > 1
-- e > 0
-- m > 1
--
-- Result is to be stored in the MutableWordArray# (which size is equal to the
-- one of m).
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
bignat_powmod
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_powmod r b e m s =
   ioVoid (ghc_bignat_powmod r b e m) s

foreign import ccall unsafe ghc_bignat_powmod
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> WordArray#
   -> IO ()

-- | \"@'bignat_powmod' /b/ /e/ /m/@\" computes base @/b/@ raised to
-- exponent @/e/@ modulo @/m/@.
--
-- b > 1
-- e > 0
-- m > 1
bignat_powmod_words
   :: Word#
   -> Word#
   -> Word#
   -> Word#
bignat_powmod_words = ghc_bignat_powmod_words

foreign import ccall unsafe ghc_bignat_powmod_words
   :: Word# -> Word# -> Word# -> Word#


-- | Return extended GCD of two non-zero integers.
--
-- I.e. integer_gcde a b returns (g,x,y) so that ax + by = g
--
-- Input: a and b are non zero.
-- Output: g must be > 0
--
integer_gcde
   :: Integer
   -> Integer
   -> (# Integer, Integer, Integer #)
integer_gcde = Native.integer_gcde
   -- for now we use Native's implementation. If some FFI backend user needs a
   -- specific implementation, we'll need to determine a prototype to pass and
   -- return BigNat signs and sizes via FFI.


-- | Computes the modular inverse of two non-zero integers.
--
-- I.e. y = integer_recip_mod x m
--        = x^(-1) `mod` m
--
-- with 0 < y < abs m
integer_recip_mod
   :: Integer
   -> Natural
   -> (# Natural | () #)
integer_recip_mod = Native.integer_recip_mod
   -- for now we use Native's implementation. If some FFI backend user needs a
   -- specific implementation, we'll need to determine a prototype to pass and
   -- return BigNat signs and sizes via FFI.

-- | Computes the modular exponentiation.
--
-- I.e. y = integer_powmod b e m
--        = b^e `mod` m
--
-- with 0 <= y < abs m
integer_powmod
   :: Integer
   -> Natural
   -> Natural
   -> Natural
integer_powmod = Native.integer_powmod
   -- for now we use Native's implementation. If some FFI backend user needs a
   -- specific implementation, we'll need to determine a prototype to pass and
   -- return BigNat signs and sizes via FFI.
