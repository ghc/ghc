{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE BlockArguments #-}

-- | Backend based on the GNU GMP library.
--
-- This has been adapted from the legacy `integer-gmp` package written by
-- Herbert Valerio Riedel.
module GHC.Num.BigNat.GMP where

#include "MachDeps.h"
#include "WordSize.h"

import GHC.Num.WordArray
import GHC.Num.Primitives
import GHC.Prim
import GHC.Types

default ()

----------------------------------------------------------------------------
-- type definitions

-- NB: all code assumes GMP_LIMB_BITS == WORD_SIZE_IN_BITS
-- The C99 code in cbits/gmp_wrappers.c will fail to compile if this doesn't hold

-- | Type representing a GMP Limb
type GmpLimb = Word -- actually, 'CULong'
type GmpLimb# = Word#

-- | Count of 'GmpLimb's, must be positive (unless specified otherwise).
type GmpSize = Int  -- actually, a 'CLong'
type GmpSize# = Int#

narrowGmpSize# :: Int# -> Int#
#if SIZEOF_LONG == SIZEOF_HSWORD
narrowGmpSize# x = x
#elif (SIZEOF_LONG == 4) && (SIZEOF_HSWORD == 8)
-- On IL32P64 (i.e. Win64), we have to be careful with CLong not being
-- 64bit.  This is mostly an issue on values returned from C functions
-- due to sign-extension.
narrowGmpSize# = narrow32Int#
#endif

narrowCInt# :: Int# -> Int#
narrowCInt# = narrow32Int#

bignat_compare :: WordArray# -> WordArray# -> Int#
bignat_compare x y = narrowCInt# (c_mpn_cmp x y (wordArraySize# x))

bignat_add
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
{-# INLINE bignat_add #-}
bignat_add mwa wa wb s
   -- weird GMP requirement
   | isTrue# (wordArraySize# wb ># wordArraySize# wa)
   = bignat_add mwa wb wa s

   | True
   = do
   case ioWord# (c_mpn_add mwa wa (wordArraySize# wa) wb (wordArraySize# wb)) s of
      (# s', c #) -> mwaWriteMostSignificant mwa c s'

bignat_add_word
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
{-# INLINE bignat_add_word #-}
bignat_add_word mwa wa b s = do
   case ioWord# (c_mpn_add_1 mwa wa (wordArraySize# wa) b) s of
      (# s', c #) -> mwaWriteMostSignificant mwa c s'

bignat_sub
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> (# State# RealWorld, Bool# #)
{-# INLINE bignat_sub #-}
bignat_sub mwa wa wb s =
   case ioWord# (c_mpn_sub mwa wa (wordArraySize# wa) wb (wordArraySize# wb)) s of
      (# s', 0## #) -> (# s', 0# #)
      (# s', _   #) -> (# s', 1# #)

bignat_sub_word
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> (# State# RealWorld, Bool# #)
{-# INLINE bignat_sub_word #-}
bignat_sub_word mwa wa b s =
   case ioWord# (c_mpn_sub_1 mwa wa (wordArraySize# wa) b) s of
      (# s', 0## #) -> (# s', 0# #)
      (# s', _   #) -> (# s', 1# #)

bignat_mul
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
{-# INLINE bignat_mul #-}
bignat_mul mwa wa wb s = do
   case ioWord# (c_mpn_mul mwa wa (wordArraySize# wa) wb (wordArraySize# wb)) s of
      (# s', _msl #) -> s' -- we don't care about the most-significant
                           -- limb. The caller shrink the mwa if
                           -- necessary anyway.

bignat_mul_word
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
{-# INLINE bignat_mul_word #-}
bignat_mul_word mwa wa b s =
   case ioWord# (c_mpn_mul_1 mwa wa (wordArraySize# wa) b) s of
      (# s', c #) -> mwaWriteMostSignificant mwa c s'

bignat_popcount :: WordArray# -> Word#
{-# INLINE bignat_popcount #-}
bignat_popcount wa = c_mpn_popcount wa (wordArraySize# wa)


bignat_shiftl
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
{-# INLINE bignat_shiftl #-}
bignat_shiftl mwa wa n s =
   case ioWord# (c_mpn_lshift mwa wa (wordArraySize# wa) n) s of
      (# s', _msl #) -> s' -- we don't care about the most-significant
                           -- limb. The caller shrink the mwa if
                           -- necessary anyway.

bignat_shiftr
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
{-# INLINE bignat_shiftr #-}
bignat_shiftr mwa wa n s =
   case ioWord# (c_mpn_rshift mwa wa (wordArraySize# wa) n) s of
      (# s', _msl #) -> s' -- we don't care about the most-significant
                           -- limb. The caller shrink the mwa if
                           -- necessary anyway.

bignat_or
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
{-# INLINE bignat_or #-}
bignat_or mwa wa wb s1
   | isTrue# (szA >=# szB) = go wa szA wb szB s1
   | True                  = go wb szB wa szA s1
   where
      !szA = wordArraySize# wa
      !szB = wordArraySize# wb
      -- nx >= ny
      go wx nx wy ny s = case ioVoid (c_mpn_ior_n mwa wx wy ny) s of
         s' -> mwaArrayCopy# mwa ny wx ny (nx -# ny) s'

bignat_xor
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
{-# INLINE bignat_xor #-}
bignat_xor mwa wa wb s1
   | isTrue# (szA >=# szB) = go wa szA wb szB s1
   | True                  = go wb szB wa szA s1
   where
      !szA = wordArraySize# wa
      !szB = wordArraySize# wb
      -- nx >= ny
      go wx nx wy ny s = case ioVoid (c_mpn_xor_n mwa wx wy ny) s of
         s' -> mwaArrayCopy# mwa ny wx ny (nx -# ny) s'

bignat_and
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
{-# INLINE bignat_and #-}
bignat_and mwa wa wb s = ioVoid (c_mpn_and_n mwa wa wb sz) s
   where
      !szA = wordArraySize# wa
      !szB = wordArraySize# wb
      !sz  = minI# szA szB

bignat_and_not
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
{-# INLINE bignat_and_not #-}
bignat_and_not mwa wa wb s =
   case ioVoid (c_mpn_andn_n mwa wa wb n) s of
      s' -> mwaArrayCopy# mwa szB wa szB (szA -# szB) s'
   where
      !szA = wordArraySize# wa
      !szB = wordArraySize# wb
      !n   = minI# szA szB

bignat_quotrem
   :: MutableWordArray# RealWorld
   -> MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_quotrem mwq mwr wa wb s =
   ioVoid (c_mpn_tdiv_qr mwq mwr 0# wa szA wb szB) s
   where
      szA = wordArraySize# wa
      szB = wordArraySize# wb

bignat_quot
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_quot mwq wa wb s =
   ioVoid (c_mpn_tdiv_q mwq wa szA wb szB) s
   where
      szA = wordArraySize# wa
      szB = wordArraySize# wb

bignat_rem
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_rem mwr wa wb s =
   ioVoid (c_mpn_tdiv_r mwr wa szA wb szB) s
   where
      szA = wordArraySize# wa
      szB = wordArraySize# wb

bignat_quotrem_word
   :: MutableWordArray# RealWorld -- ^ Quotient
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> (# State# RealWorld, Word# #)
bignat_quotrem_word mwq wa b s =
   ioWord# (c_mpn_divrem_1 mwq 0# wa szA b) s
   where
      szA = wordArraySize# wa

bignat_quot_word
   :: MutableWordArray# RealWorld -- ^ Quotient
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
bignat_quot_word mwq wa b s =
   case bignat_quotrem_word mwq wa b s of
      (# s', _ #) -> s'

bignat_rem_word
   :: WordArray#
   -> Word#
   -> Word#
bignat_rem_word wa b =
   c_mpn_mod_1 wa (wordArraySize# wa) b


bignat_gcd
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_gcd mwr wa wb s =
   -- wa > wb
   case ioInt# (c_mpn_gcd# mwr wa (wordArraySize# wa) wb (wordArraySize# wb)) s of
      (# s', sz #) -> mwaSetSize# mwr (narrowGmpSize# sz) s'

bignat_gcd_word
   :: WordArray#
   -> Word#
   -> Word#
bignat_gcd_word wa b = c_mpn_gcd_1# wa (wordArraySize# wa) b

bignat_gcd_word_word
   :: Word#
   -> Word#
   -> Word#
bignat_gcd_word_word = integer_gmp_gcd_word


bignat_encode_double :: WordArray# -> Int# -> Double#
bignat_encode_double wa e = c_mpn_get_d wa (wordArraySize# wa) e

bignat_shiftr_neg
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
bignat_shiftr_neg mwa wa n s =
   ioVoid (c_mpn_rshift_2c mwa wa (wordArraySize# wa) n) s

bignat_powmod_word
   :: WordArray#
   -> WordArray#
   -> Word#
   -> Word#
bignat_powmod_word b e m =
   integer_gmp_powm1# b (wordArraySize# b) e (wordArraySize# e) m

bignat_powmod_words
   :: Word#
   -> Word#
   -> Word#
   -> Word#
bignat_powmod_words = integer_gmp_powm_word

bignat_powmod
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_powmod r b e m s =
   ioVoid (integer_gmp_powm# r b (wordArraySize# b) e (wordArraySize# e) m (wordArraySize# m)) s


----------------------------------------------------------------------
-- FFI ccall imports

foreign import ccall unsafe "integer_gmp_gcd_word"
  integer_gmp_gcd_word :: GmpLimb# -> GmpLimb# -> GmpLimb#

foreign import ccall unsafe "integer_gmp_mpn_gcd_1"
  c_mpn_gcd_1# :: ByteArray# -> GmpSize# -> GmpLimb# -> GmpLimb#

foreign import ccall unsafe "integer_gmp_mpn_gcd"
  c_mpn_gcd# :: MutableByteArray# s -> ByteArray# -> GmpSize#
                -> ByteArray# -> GmpSize# -> IO GmpSize

foreign import ccall unsafe "integer_gmp_gcdext"
  integer_gmp_gcdext# :: MutableByteArray# s -> MutableByteArray# s
                         -> ByteArray# -> GmpSize#
                         -> ByteArray# -> GmpSize# -> IO GmpSize

-- mp_limb_t mpn_add_1 (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t n,
--                      mp_limb_t s2limb)
foreign import ccall unsafe "gmp.h __gmpn_add_1"
  c_mpn_add_1 :: MutableByteArray# s -> ByteArray# -> GmpSize# -> GmpLimb#
                 -> IO GmpLimb

-- mp_limb_t mpn_sub_1 (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t n,
--                      mp_limb_t s2limb)
foreign import ccall unsafe "gmp.h __gmpn_sub_1"
  c_mpn_sub_1 :: MutableByteArray# s -> ByteArray# -> GmpSize# -> GmpLimb#
                 -> IO GmpLimb

-- mp_limb_t mpn_mul_1 (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t n,
--                      mp_limb_t s2limb)
foreign import ccall unsafe "gmp.h __gmpn_mul_1"
  c_mpn_mul_1 :: MutableByteArray# s -> ByteArray# -> GmpSize# -> GmpLimb#
                 -> IO GmpLimb

-- mp_limb_t mpn_add (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t s1n,
--                    const mp_limb_t *s2p, mp_size_t s2n)
foreign import ccall unsafe "gmp.h __gmpn_add"
  c_mpn_add :: MutableByteArray# s -> ByteArray# -> GmpSize#
               -> ByteArray# -> GmpSize# -> IO GmpLimb

-- mp_limb_t mpn_sub (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t s1n,
--                    const mp_limb_t *s2p, mp_size_t s2n)
foreign import ccall unsafe "gmp.h __gmpn_sub"
  c_mpn_sub :: MutableByteArray# s -> ByteArray# -> GmpSize# -> ByteArray#
               -> GmpSize# -> IO GmpLimb

-- mp_limb_t mpn_mul (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t s1n,
--                    const mp_limb_t *s2p, mp_size_t s2n)
foreign import ccall unsafe "gmp.h __gmpn_mul"
  c_mpn_mul :: MutableByteArray# s -> ByteArray# -> GmpSize# -> ByteArray#
               -> GmpSize# -> IO GmpLimb

-- int mpn_cmp (const mp_limb_t *s1p, const mp_limb_t *s2p, mp_size_t n)
foreign import ccall unsafe "gmp.h __gmpn_cmp"
  c_mpn_cmp :: ByteArray# -> ByteArray# -> GmpSize# -> Int#

-- void mpn_tdiv_qr (mp_limb_t *qp, mp_limb_t *rp, mp_size_t qxn,
--                   const mp_limb_t *np, mp_size_t nn,
--                   const mp_limb_t *dp, mp_size_t dn)
foreign import ccall unsafe "gmp.h __gmpn_tdiv_qr"
  c_mpn_tdiv_qr :: MutableByteArray# s -> MutableByteArray# s -> GmpSize#
                   -> ByteArray# -> GmpSize# -> ByteArray# -> GmpSize# -> IO ()

foreign import ccall unsafe "integer_gmp_mpn_tdiv_q"
  c_mpn_tdiv_q :: MutableByteArray# s -> ByteArray# -> GmpSize# -> ByteArray#
                  -> GmpSize# -> IO ()

foreign import ccall unsafe "integer_gmp_mpn_tdiv_r"
  c_mpn_tdiv_r :: MutableByteArray# s -> ByteArray# -> GmpSize# -> ByteArray#
                  -> GmpSize# -> IO ()

-- mp_limb_t mpn_divrem_1 (mp_limb_t *r1p, mp_size_t qxn, mp_limb_t *s2p,
--                         mp_size_t s2n, mp_limb_t s3limb)
foreign import ccall unsafe "gmp.h __gmpn_divrem_1"
  c_mpn_divrem_1 :: MutableByteArray# s -> GmpSize# -> ByteArray# -> GmpSize#
                    -> GmpLimb# -> IO GmpLimb

-- mp_limb_t mpn_mod_1 (const mp_limb_t *s1p, mp_size_t s1n, mp_limb_t s2limb)
foreign import ccall unsafe "gmp.h __gmpn_mod_1"
  c_mpn_mod_1 :: ByteArray# -> GmpSize# -> GmpLimb# -> GmpLimb#

-- mp_limb_t integer_gmp_mpn_rshift (mp_limb_t rp[], const mp_limb_t sp[],
--                                   mp_size_t sn, mp_bitcnt_t count)
foreign import ccall unsafe "integer_gmp_mpn_rshift"
  c_mpn_rshift :: MutableByteArray# s -> ByteArray# -> GmpSize# -> Word#
                  -> IO GmpLimb

-- mp_limb_t integer_gmp_mpn_rshift (mp_limb_t rp[], const mp_limb_t sp[],
--                                   mp_size_t sn, mp_bitcnt_t count)
foreign import ccall unsafe "integer_gmp_mpn_rshift_2c"
  c_mpn_rshift_2c :: MutableByteArray# s -> ByteArray# -> GmpSize# -> Word#
                     -> IO GmpLimb

-- mp_limb_t integer_gmp_mpn_lshift (mp_limb_t rp[], const mp_limb_t sp[],
--                                   mp_size_t sn, mp_bitcnt_t count)
foreign import ccall unsafe "integer_gmp_mpn_lshift"
  c_mpn_lshift :: MutableByteArray# s -> ByteArray# -> GmpSize# -> Word#
                  -> IO GmpLimb

-- void mpn_and_n (mp_limb_t *rp, const mp_limb_t *s1p, const mp_limb_t *s2p,
--                 mp_size_t n)
foreign import ccall unsafe "integer_gmp_mpn_and_n"
  c_mpn_and_n :: MutableByteArray# s -> ByteArray# -> ByteArray# -> GmpSize#
                 -> IO ()

-- void mpn_andn_n (mp_limb_t *rp, const mp_limb_t *s1p, const mp_limb_t *s2p,
--                  mp_size_t n)
foreign import ccall unsafe "integer_gmp_mpn_andn_n"
  c_mpn_andn_n :: MutableByteArray# s -> ByteArray# -> ByteArray# -> GmpSize#
                  -> IO ()

-- void mpn_ior_n (mp_limb_t *rp, const mp_limb_t *s1p, const mp_limb_t *s2p,
--                 mp_size_t n)
foreign import ccall unsafe "integer_gmp_mpn_ior_n"
  c_mpn_ior_n :: MutableByteArray# s -> ByteArray# -> ByteArray# -> GmpSize#
                 -> IO ()

-- void mpn_xor_n (mp_limb_t *rp, const mp_limb_t *s1p, const mp_limb_t *s2p,
--                 mp_size_t n)
foreign import ccall unsafe "integer_gmp_mpn_xor_n"
  c_mpn_xor_n :: MutableByteArray# s -> ByteArray# -> ByteArray# -> GmpSize#
                 -> IO ()

-- mp_bitcnt_t mpn_popcount (const mp_limb_t *s1p, mp_size_t n)
foreign import ccall unsafe "gmp.h __gmpn_popcount"
  c_mpn_popcount :: ByteArray# -> GmpSize# -> Word#

-- double integer_gmp_mpn_get_d (const mp_limb_t sp[], const mp_size_t sn)
foreign import ccall unsafe "integer_gmp_mpn_get_d"
  c_mpn_get_d :: ByteArray# -> GmpSize# -> Int# -> Double#

foreign import ccall unsafe "integer_gmp_powm"
  integer_gmp_powm# :: MutableByteArray# RealWorld
                       -> ByteArray# -> GmpSize# -> ByteArray# -> GmpSize#
                       -> ByteArray# -> GmpSize# -> IO GmpSize

foreign import ccall unsafe "integer_gmp_powm_word"
  integer_gmp_powm_word :: GmpLimb# -> GmpLimb# -> GmpLimb# -> GmpLimb#

foreign import ccall unsafe "integer_gmp_powm1"
  integer_gmp_powm1# :: ByteArray# -> GmpSize# -> ByteArray# -> GmpSize#
                        -> GmpLimb# -> GmpLimb#
