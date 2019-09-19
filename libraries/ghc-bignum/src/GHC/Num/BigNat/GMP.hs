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

module GHC.Num.BigNat.GMP where

#include "MachDeps.h"
#include "HsIntegerGmp.h"

import GHC.Num.WordArray
import GHC.Num.Primitives
import GHC.Prim
import GHC.Types
#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif


-- Sanity check as CPP defines are implicitly 0-valued when undefined
#if !(defined(SIZEOF_LONG) && defined(SIZEOF_HSWORD) \
           && defined(WORD_SIZE_IN_BITS))
# error missing defines
#endif


default ()

----------------------------------------------------------------------------
-- type definitions

-- NB: all code assumes GMP_LIMB_BITS == WORD_SIZE_IN_BITS
-- The C99 code in cbits/gmp_wrappers.c will fail to compile if this doesn't hold

#if WORD_SIZE_IN_BITS == 64
# define GMP_LIMB_SHIFT   3
# define GMP_LIMB_BYTES   8
# define GMP_LIMB_BITS    64
# define INT_MINBOUND     -0x8000000000000000
# define INT_MAXBOUND      0x7fffffffffffffff
# define ABS_INT_MINBOUND  0x8000000000000000
# define SQRT_INT_MAXBOUND 0xb504f333
#elif WORD_SIZE_IN_BITS == 32
# define GMP_LIMB_SHIFT   2
# define GMP_LIMB_BYTES   4
# define GMP_LIMB_BITS    32
# define INT_MINBOUND     -0x80000000
# define INT_MAXBOUND      0x7fffffff
# define ABS_INT_MINBOUND  0x80000000
# define SQRT_INT_MAXBOUND 0xb504
#else
# error unsupported WORD_SIZE_IN_BITS config
#endif


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
                     -- TODO: why narrow?

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
      (# s', c #) -> mwaWriteMostSignificant' mwa c s'

bignat_add_word
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
{-# INLINE bignat_add_word #-}
bignat_add_word mwa wa b s = do
   case ioWord# (c_mpn_add_1 mwa wa (wordArraySize# wa) b) s of
      (# s', c #) -> mwaWriteMostSignificant' mwa c s'

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
      (# s', c #) -> mwaWriteMostSignificant' mwa c s'

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

-- -- internal non-exported helper
-- powModSBigNatWord :: SBigNat -> SBigNat -> GmpLimb# -> GmpLimb#
-- powModSBigNatWord b e m# = integer_gmp_powm1# b# bn# e# en# m#
--   where
--     !(BN# b#) = absSBigNat b
--     !(BN# e#) = absSBigNat e
--     bn# = ssizeofSBigNat# b
--     en# = ssizeofSBigNat# e
-- 

-- -- internal non-exported helper
-- powModSBigNat :: SBigNat -> SBigNat -> BigNat -> BigNat
-- powModSBigNat b e m@(BN# m#) = runS $ do
--     r@(MBN# r#) <- newBigNat# mn#
--     I# rn_# <- liftIO (integer_gmp_powm# r# b# bn# e# en# m# mn#)
--     let rn# = narrowGmpSize# rn_#
--     case isTrue# (rn# ==# mn#) of
--         False -> unsafeShrinkFreezeBigNat# r rn#
--         True  -> unsafeFreezeBigNat# r
--   where
--     !(BN# b#) = absSBigNat b
--     !(BN# e#) = absSBigNat e
--     bn# = ssizeofSBigNat# b
--     en# = ssizeofSBigNat# e
--     mn# = sizeofBigNat# m
-- 
-- 
-- -- internal non-exported helper
-- powModSecSBigNat :: SBigNat -> SBigNat -> BigNat -> BigNat
-- powModSecSBigNat b e m@(BN# m#) = runS $ do
--     r@(MBN# r#) <- newBigNat# mn#
--     I# rn_# <- liftIO (integer_gmp_powm_sec# r# b# bn# e# en# m# mn#)
--     let rn# = narrowGmpSize# rn_#
--     case isTrue# (rn# ==# mn#) of
--         False -> unsafeShrinkFreezeBigNat# r rn#
--         True  -> unsafeFreezeBigNat# r
--   where
--     !(BN# b#) = absSBigNat b
--     !(BN# e#) = absSBigNat e
--     bn# = ssizeofSBigNat# b
--     en# = ssizeofSBigNat# e
--     mn# = sizeofBigNat# m
-- 
-- -- | Version of 'recipModInteger' operating on 'BigNat's
-- --
-- -- @since 1.0.0.0
-- recipModBigNat :: BigNat -> BigNat -> BigNat
-- recipModBigNat x m = inline recipModSBigNat (PosBN x) m
-- 
-- -- | Version of 'recipModInteger' operating on 'Word#'s
-- --
-- -- @since 1.0.0.0
-- foreign import ccall unsafe "integer_gmp_invert_word"
--   recipModWord :: GmpLimb# -> GmpLimb# -> GmpLimb#
-- 
-- -- internal non-exported helper
-- recipModSBigNat :: SBigNat -> BigNat -> BigNat
-- recipModSBigNat x m@(BN# m#) = runS $ do
--     r@(MBN# r#) <- newBigNat# mn#
--     I# rn_# <- liftIO (integer_gmp_invert# r# x# xn# m# mn#)
--     let rn# = narrowGmpSize# rn_#
--     case isTrue# (rn# ==# mn#) of
--         False -> unsafeShrinkFreezeBigNat# r rn#
--         True  -> unsafeFreezeBigNat# r
--   where
--     !(BN# x#) = absSBigNat x
--     xn# = ssizeofSBigNat# x
--     mn# = sizeofBigNat# m
-- 
-- foreign import ccall unsafe "integer_gmp_invert"
--   integer_gmp_invert# :: MutableByteArray# RealWorld
--                          -> ByteArray# -> GmpSize#
--                          -> ByteArray# -> GmpSize# -> IO GmpSize
--
-- -- | Extended euclidean algorithm.
-- --
-- -- For @/a/@ and @/b/@, compute their greatest common divisor @/g/@
-- -- and the coefficient @/s/@ satisfying @/a//s/ + /b//t/ = /g/@.
-- --
-- -- @since 0.5.1.0
-- gcdExtSBigNat :: SBigNat -> SBigNat -> (# BigNat, SBigNat #)
-- gcdExtSBigNat x y = case runS go of (g,s) -> (# g, s #)
--   where
--     go = do
--         g@(MBN# g#) <- newBigNat# gn0#
--         -- According to https://gmplib.org/manual/Number-Theoretic-Functions.html#index-mpz_005fgcdext
--         -- abs(s) < abs(y) / (2 g)
--         s@(MBN# s#) <- newBigNat# (absI# yn#)
--         I# ssn_# <- liftIO (integer_gmp_gcdext# s# g# x# xn# y# yn#)
--         let ssn# = narrowGmpSize# ssn_#
--             sn#  = absI# ssn#
--         s' <- unsafeShrinkFreezeBigNat# s sn#
--         g' <- unsafeRenormFreezeBigNat# g
--         case isTrue# (ssn# >=# 0#) of
--             False -> return ( g', NegBN s' )
--             True  -> return ( g', PosBN s' )
-- 
--     !(BN# x#) = absSBigNat x
--     !(BN# y#) = absSBigNat y
--     xn# = ssizeofSBigNat# x
--     yn# = ssizeofSBigNat# y
-- 
--     gn0# = minI# (absI# xn#) (absI# yn#)
-- 

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

foreign import ccall unsafe "integer_gmp_powm_sec"
  integer_gmp_powm_sec# :: MutableByteArray# RealWorld
                           -> ByteArray# -> GmpSize# -> ByteArray# -> GmpSize#
                           -> ByteArray# -> GmpSize# -> IO GmpSize



-- -- | Construct 'BigNat' from existing 'ByteArray#' containing /n/
-- -- 'GmpLimb's in least-significant-first order.
-- --
-- -- If possible 'ByteArray#', will be used directly (i.e. shared
-- -- /without/ cloning the 'ByteArray#' into a newly allocated one)
-- --
-- -- Note: size parameter (times @sizeof(GmpLimb)@) must be less or
-- -- equal to its 'sizeofByteArray#'.
-- byteArrayToBigNat# :: ByteArray# -> GmpSize# -> BigNat
-- byteArrayToBigNat# ba# n0#
--   | isTrue# (n#  ==# 0#)    = zeroBigNat
--   | isTrue# (baszr# ==# 0#) -- i.e. ba# is multiple of limb-size
--   , isTrue# (baszq# ==# n#) = (BN# ba#)
--   | True = runS $ \s ->
--       let !(# s', mbn@(MBN# mba#) #) = newBigNat# n# s
--           !(# s'', ba_sz# #) = getSizeofMutableByteArray# mba# s'
--           go = do _ <- svoid (copyByteArray# ba# 0# mba# 0# ba_sz# )
--                   unsafeFreezeBigNat# mbn
--       in go s''
--   where
--     !(# baszq#, baszr# #) = quotRemInt# (sizeofByteArray# ba#) GMP_LIMB_BYTES#
-- 
--     n#  = fmssl (BN# ba#) (n0# -# 1#)
-- 
-- -- | Version of 'importIntegerFromAddr' constructing a 'BigNat'
-- importBigNatFromAddr :: Addr# -> Word# -> Int# -> IO BigNat
-- importBigNatFromAddr _ 0## _ = IO (\s -> (# s, zeroBigNat #))
-- importBigNatFromAddr addr len0 1# = IO $ do -- MSBF
--     W# ofs <- liftIO (c_scan_nzbyte_addr addr 0## len0)
--     let len = len0 `minusWord#` ofs
--         addr' = addr `plusAddr#` (word2Int# ofs)
--     importBigNatFromAddr# addr' len 1#
-- importBigNatFromAddr addr len0 _ = IO $ do -- LSBF
--     W# len <- liftIO (c_rscan_nzbyte_addr addr 0## len0)
--     importBigNatFromAddr# addr len 0#
-- 
-- foreign import ccall unsafe "integer_gmp_scan_nzbyte"
--     c_scan_nzbyte_addr :: Addr# -> Word# -> Word# -> IO Word
-- 
-- foreign import ccall unsafe "integer_gmp_rscan_nzbyte"
--     c_rscan_nzbyte_addr :: Addr# -> Word# -> Word# -> IO Word
-- 
-- -- | Helper for 'importBigNatFromAddr'
-- importBigNatFromAddr# :: Addr# -> Word# -> Int# -> S RealWorld BigNat
-- importBigNatFromAddr# _ 0## _ = return zeroBigNat
-- importBigNatFromAddr# addr len msbf = do
--     mbn@(MBN# mba#) <- newBigNat# n#
--     () <- liftIO (c_mpn_import_addr mba# addr 0## len msbf)
--     unsafeFreezeBigNat# mbn
--   where
--     -- n = ceiling(len / SIZEOF_HSWORD), i.e. number of limbs required
--     n# = (word2Int# len +# (SIZEOF_HSWORD# -# 1#)) `quotInt#` SIZEOF_HSWORD#
-- 
-- foreign import ccall unsafe "integer_gmp_mpn_import"
--     c_mpn_import_addr :: MutableByteArray# RealWorld -> Addr# -> Word# -> Word#
--                       -> Int# -> IO ()
-- 
-- -- | Version of 'importIntegerFromByteArray' constructing a 'BigNat'
-- importBigNatFromByteArray :: ByteArray# -> Word# -> Word# -> Int# -> BigNat
-- importBigNatFromByteArray _  _    0##  _  = zeroBigNat
-- importBigNatFromByteArray ba ofs0 len0 1# = runS $ do -- MSBF
--     W# ofs <- liftIO (c_scan_nzbyte_bytearray ba ofs0 len0)
--     let len = (len0 `plusWord#` ofs0) `minusWord#` ofs
--     importBigNatFromByteArray# ba ofs len 1#
-- importBigNatFromByteArray ba ofs  len0 _  = runS $ do -- LSBF
--     W# len <- liftIO (c_rscan_nzbyte_bytearray ba ofs len0)
--     importBigNatFromByteArray# ba ofs len 0#
-- 
-- foreign import ccall unsafe "integer_gmp_scan_nzbyte"
--     c_scan_nzbyte_bytearray :: ByteArray# -> Word# -> Word# -> IO Word
-- 
-- foreign import ccall unsafe "integer_gmp_rscan_nzbyte"
--     c_rscan_nzbyte_bytearray :: ByteArray# -> Word# -> Word# -> IO Word
-- 
-- -- | Helper for 'importBigNatFromByteArray'
-- importBigNatFromByteArray# :: ByteArray# -> Word# -> Word# -> Int#
--                            -> S RealWorld BigNat
-- importBigNatFromByteArray# _ _ 0## _ = return zeroBigNat
-- importBigNatFromByteArray# ba ofs len msbf = do
--     mbn@(MBN# mba#) <- newBigNat# n#
--     () <- liftIO (c_mpn_import_bytearray mba# ba ofs len msbf)
--     unsafeFreezeBigNat# mbn
--   where
--     -- n = ceiling(len / SIZEOF_HSWORD), i.e. number of limbs required
--     n# = (word2Int# len +# (SIZEOF_HSWORD# -# 1#)) `quotInt#` SIZEOF_HSWORD#
-- 
-- foreign import ccall unsafe "integer_gmp_mpn_import"
--     c_mpn_import_bytearray :: MutableByteArray# RealWorld -> ByteArray# -> Word#
--                            -> Word# -> Int# -> IO ()
-- 
-- -- | Version of 'nextPrimeInteger' operating on 'BigNat's
-- --
-- -- @since 1.0.0.0
-- nextPrimeBigNat :: BigNat -> BigNat
-- nextPrimeBigNat bn@(BN# ba#) = runS $ do
--     mbn@(MBN# mba#) <- newBigNat# n#
--     (W# c#) <- liftIO (nextPrime# mba# ba# n#)
--     case c# of
--         0## -> unsafeFreezeBigNat# mbn
--         _   -> unsafeSnocFreezeBigNat# mbn c#
--   where
--     n# = sizeofBigNat# bn
-- 
-- foreign import ccall unsafe "integer_gmp_next_prime"
--   nextPrime# :: MutableByteArray# RealWorld -> ByteArray# -> GmpSize#
--                 -> IO GmpLimb
-- 
-- ----------------------------------------------------------------------------
-- 
-- -- | Internal helper type for "signed" 'BigNat's
-- --
-- -- This is a useful abstraction for operations which support negative
-- -- mp_size_t arguments.
-- data SBigNat = NegBN !BigNat | PosBN !BigNat
-- 
-- -- | Absolute value of 'SBigNat'
-- absSBigNat :: SBigNat -> BigNat
-- absSBigNat (NegBN bn) = bn
-- absSBigNat (PosBN bn) = bn
-- 
-- -- | /Signed/ limb count. Negative sizes denote negative integers
-- ssizeofSBigNat# :: SBigNat -> GmpSize#
-- ssizeofSBigNat# (NegBN bn) = negateInt# (sizeofBigNat# bn)
-- ssizeofSBigNat# (PosBN bn) = sizeofBigNat# bn
-- 
-- -- | Construct 'SBigNat' from 'Int#' value
-- intToSBigNat# :: Int# -> SBigNat
-- intToSBigNat# 0#     = PosBN zeroBigNat
-- intToSBigNat# 1#     = PosBN oneBigNat
-- intToSBigNat# (-1#)  = NegBN oneBigNat
-- intToSBigNat# i# | isTrue# (i# ># 0#) = PosBN (wordToBigNat (int2Word# i#))
--                  | True   = NegBN (wordToBigNat (int2Word# (negateInt# i#)))
-- 
-- 
-- -- find most-sig set limb, starting at given index
-- fmssl :: BigNat -> Int# -> Int#
-- fmssl !bn i0# = go i0#
--   where
--     go i# | isTrue# (i# <# 0#)                         = 0#
--           | isTrue# (neWord# (indexBigNat# bn i#) 0##) = i# +# 1#
--           | True                                       = go (i# -# 1#)
-- 
-- -- | Version of 'exportIntegerToAddr' operating on 'BigNat's.
-- exportBigNatToAddr :: BigNat -> Addr# -> Int# -> IO Word
-- exportBigNatToAddr bn@(BN# ba#) addr e
--   = c_mpn_exportToAddr# ba# (sizeofBigNat# bn) addr 0# e
-- 
-- foreign import ccall unsafe "integer_gmp_mpn_export"
--   c_mpn_exportToAddr# :: ByteArray# -> GmpSize# -> Addr# -> Int# -> Int#
--                          -> IO Word
-- 
-- shiftRNegBigNat :: BigNat -> Int# -> BigNat
-- shiftRNegBigNat x 0# = x
-- shiftRNegBigNat x _ | isZeroBigNat x = zeroBigNat
-- shiftRNegBigNat x@(BN# xba#) n#
--   | isTrue# (nlimbs# >=# xn#) = zeroBigNat
--   | True = runS $ do
--       ymbn@(MBN# ymba#) <- newBigNat# yn#
--       W# ymsl <- liftIO (c_mpn_rshift_2c ymba# xba# xn# (int2Word# n#))
--       case ymsl of
--           0## -> unsafeRenormFreezeBigNat# ymbn -- may shrink more than one
--           _   -> unsafeFreezeBigNat# ymbn
--   where
--     xn# = sizeofBigNat# x
--     yn# = xn# -# nlimbs#
--     nlimbs# = quotInt# (n# -# 1#) GMP_LIMB_BITS#
-- 
-- -- | Version of 'exportIntegerToAddr' operating on 'Word's.
-- exportWordToAddr :: Word -> Addr# -> Int# -> IO Word
-- exportWordToAddr (W# w#) addr
--   = c_mpn_export1ToAddr# w# addr 0# -- TODO: we don't calling GMP for that
-- 
-- foreign import ccall unsafe "integer_gmp_mpn_export1"
--   c_mpn_export1ToAddr# :: GmpLimb# -> Addr# -> Int# -> Int#
--                           -> IO Word
-- 
-- -- | Version of 'exportIntegerToMutableByteArray' operating on 'BigNat's.
-- --
-- -- @since 1.0.0.0
-- exportBigNatToMutableByteArray :: BigNat -> MutableByteArray# RealWorld -> Word#
--                                -> Int# -> IO Word
-- exportBigNatToMutableByteArray bn@(BN# ba#)
--   = c_mpn_exportToMutableByteArray# ba# (sizeofBigNat# bn)
-- 
-- foreign import ccall unsafe "integer_gmp_mpn_export"
--   c_mpn_exportToMutableByteArray# :: ByteArray# -> GmpSize#
--                                   -> MutableByteArray# RealWorld -> Word#
--                                   -> Int# -> IO Word
-- 
-- -- | Version of 'exportIntegerToMutableByteArray' operating on 'Word's.
-- --
-- -- @since 1.0.0.0
-- exportWordToMutableByteArray :: Word -> MutableByteArray# RealWorld -> Word#
--                              -> Int# -> IO Word
-- exportWordToMutableByteArray (W# w#) = c_mpn_export1ToMutableByteArray# w#
-- 
-- foreign import ccall unsafe "integer_gmp_mpn_export1"
--   c_mpn_export1ToMutableByteArray# :: GmpLimb# -> MutableByteArray# RealWorld
--                                    -> Word# -> Int# -> IO Word
-- 
-- 
-- -- | Version of 'testPrimeInteger' operating on 'BigNat's
-- --
-- -- @since 1.0.0.0
-- testPrimeBigNat :: BigNat -> Int# -> Int#
-- testPrimeBigNat bn@(BN# ba#) = c_integer_gmp_test_prime# ba# (sizeofBigNat# bn)
-- 
-- foreign import ccall unsafe "integer_gmp_test_prime"
--   c_integer_gmp_test_prime# :: ByteArray# -> GmpSize# -> Int# -> Int#
