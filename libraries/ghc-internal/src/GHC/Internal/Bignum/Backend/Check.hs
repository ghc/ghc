{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Check Native implementation against another backend
module GHC.Internal.Bignum.Backend.Check where

import GHC.CString
import GHC.Prim
import GHC.Types
import GHC.Internal.Bignum.WordArray
import GHC.Internal.Bignum.Primitives
import {-# SOURCE #-} GHC.Internal.Bignum.Integer
import {-# SOURCE #-} GHC.Internal.Bignum.Natural
import qualified GHC.Internal.Bignum.Backend.Native   as Native
import qualified GHC.Internal.Bignum.Backend.Selected as Other

#if defined(BIGNUM_NATIVE)
#error You can't validate Native backend against itself. Choose another backend (e.g. gmp, ffi)
#endif

default ()

-- | ghc-bignum backend name
backendName :: [Char]
backendName = unpackAppendCString# "check-"# Other.backendName
  -- we don't have (++) at our disposal, so we directly use
  -- `unpackAppendCString#`

bignat_compare
   :: WordArray#
   -> WordArray#
   -> Int#
bignat_compare a b =
   let
      gr = Other.bignat_compare a b
      nr = Native.bignat_compare a b
   in case gr ==# nr of
         0# -> unexpectedValue_Int# (# #)
         _  -> gr

mwaCompare
   :: MutableWordArray# s
   -> MutableWordArray# s
   -> State# s
   -> (# State# s, Bool# #)
mwaCompare mwa mwb s =
   case mwaSize# mwa s of
      (# s, szA #) -> case mwaSize# mwb s of
         (# s, szB #) -> case szA ==# szB of
            0# -> (# s, 0# #)
            _  -> let
                     go i s
                        | isTrue# (i <# 0#) = (# s, 1# #)
                        | True =
                           case readWordArray# mwa i s of
                              (# s, a #) -> case readWordArray# mwb i s of
                                 (# s, b #) -> case a `eqWord#` b of
                                    0# -> (# s, 0# #)
                                    _  -> go (i -# 1#) s
                  in go (szA -# 1#) s

mwaCompareOp
   :: MutableWordArray# s
   -> (MutableWordArray# s -> State# s -> State# s)
   -> (MutableWordArray# s -> State# s -> State# s)
   -> State# s
   -> State# s
mwaCompareOp mwa f g s =
   case mwaSize# mwa s of { (# s, sz #) ->
   case newWordArray# sz s of { (# s, mwb #) ->
   case f mwa s of { s ->
   case g mwb s of { s ->
   case mwaTrimZeroes# mwa s of { s ->
   case mwaTrimZeroes# mwb s of { s ->
   case mwaCompare mwa mwb s of
      (# s, 0# #) -> case unexpectedValue of
                        !_ -> s
                        -- see Note [ghc-bignum exceptions] in
                        -- GHC.Num.Primitives
      (# s, _  #) -> s
   }}}}}}

mwaCompareOp2
   :: MutableWordArray# s
   -> MutableWordArray# s
   -> (MutableWordArray# s -> MutableWordArray# s -> State# s -> State# s)
   -> (MutableWordArray# s -> MutableWordArray# s -> State# s -> State# s)
   -> State# s
   -> State# s
mwaCompareOp2 mwa mwb f g s =
   case mwaSize# mwa s of { (# s, szA #) ->
   case mwaSize# mwb s of { (# s, szB #) ->
   case newWordArray# szA s of { (# s, mwa' #) ->
   case newWordArray# szB s of { (# s, mwb' #) ->
   case f mwa  mwb  s of { s ->
   case g mwa' mwb' s of { s ->
   case mwaTrimZeroes# mwa s of { s ->
   case mwaTrimZeroes# mwb s of { s ->
   case mwaTrimZeroes# mwa' s of { s ->
   case mwaTrimZeroes# mwb' s of { s ->
   case mwaCompare mwa mwa' s of { (# s, ba #) ->
   case mwaCompare mwb mwb' s of { (# s, bb #) ->
   case ba &&# bb of
      0# -> case unexpectedValue of
               !_ -> s
               -- see Note [ghc-bignum exceptions] in GHC.Num.Primitives
      _  -> s
   }}}}}}}}}}}}

mwaCompareOpBool
   :: MutableWordArray# s
   -> (MutableWordArray# s -> State# s -> (#State# s, Bool# #))
   -> (MutableWordArray# s -> State# s -> (#State# s, Bool# #))
   -> State# s
   -> (# State# s, Bool# #)
mwaCompareOpBool mwa f g s =
   case mwaSize# mwa s of { (# s, sz #) ->
   case newWordArray# sz s of { (# s, mwb #) ->
   case f mwa s of { (# s, ra #) ->
   case g mwb s of { (# s, rb #) ->
   case ra ==# rb of
      0# -> case unexpectedValue of
               !_ -> (# s, ra #)
               -- see Note [ghc-bignum exceptions] in GHC.Num.Primitives
      _  -> case ra of -- don't compare MWAs if underflow signaled!
         0# -> (# s, ra #) -- underflow
         _  -> case mwaTrimZeroes# mwa s of { s ->
               case mwaTrimZeroes# mwb s of { s ->
               case mwaCompare mwa mwb s of
                  (# s, 0# #) -> case unexpectedValue of
                                    !_ -> (# s, ra #)
                                    -- see Note [ghc-bignum exceptions] in
                                    -- GHC.Num.Primitives
                  _  -> (# s, ra #)
   }}}}}}

mwaCompareOpWord
   :: MutableWordArray# s
   -> (MutableWordArray# s -> State# s -> (#State# s, Word# #))
   -> (MutableWordArray# s -> State# s -> (#State# s, Word# #))
   -> State# s
   -> (# State# s, Word# #)
mwaCompareOpWord mwa f g s =
   case mwaSize# mwa s of { (# s, sz #) ->
   case newWordArray# sz s of { (# s, mwb #) ->
   case f mwa s of { (# s, ra #) ->
   case g mwb s of { (# s, rb #) ->
   case mwaTrimZeroes# mwa s of { s ->
   case mwaTrimZeroes# mwb s of { s ->
   case mwaCompare mwa mwb s of
      (# s, b #) -> case b &&# (ra `eqWord#` rb) of
         0# -> case unexpectedValue of
                  !_ -> (# s, ra #)
                  -- see Note [ghc-bignum exceptions] in GHC.Num.Primitives
         _  -> (# s, ra #)
   }}}}}}

bignat_add
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_add mwa wa wb
   = mwaCompareOp mwa
      (\m -> Other.bignat_add m wa wb)
      (\m -> Native.bignat_add m wa wb)

bignat_add_word
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
bignat_add_word mwa wa b
   = mwaCompareOp mwa
      (\m -> Other.bignat_add_word m wa b)
      (\m -> Native.bignat_add_word m wa b)

bignat_mul_word
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
bignat_mul_word mwa wa b
   = mwaCompareOp mwa
      (\m -> Other.bignat_mul_word m wa b)
      (\m -> Native.bignat_mul_word m wa b)

bignat_sub
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> (# State# RealWorld, Bool# #)
bignat_sub mwa wa wb
   = mwaCompareOpBool mwa
      (\m -> Other.bignat_sub m wa wb)
      (\m -> Native.bignat_sub m wa wb)

bignat_sub_word
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> (# State# RealWorld, Bool# #)
bignat_sub_word mwa wa b
   = mwaCompareOpBool mwa
      (\m -> Other.bignat_sub_word m wa b)
      (\m -> Native.bignat_sub_word m wa b)

bignat_mul
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_mul mwa wa wb
   = mwaCompareOp mwa
      (\m -> Other.bignat_mul m wa wb)
      (\m -> Native.bignat_mul m wa wb)

bignat_popcount :: WordArray# -> Word#
bignat_popcount wa =
   let
      gr = Other.bignat_popcount wa
      nr = Native.bignat_popcount wa
   in case gr `eqWord#` nr of
         0# -> 1## `quotWord#` 0##
         _  -> gr

bignat_shiftl
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
bignat_shiftl mwa wa n
   = mwaCompareOp mwa
      (\m -> Other.bignat_shiftl m wa n)
      (\m -> Native.bignat_shiftl m wa n)

bignat_shiftr
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
bignat_shiftr mwa wa n
   = mwaCompareOp mwa
      (\m -> Other.bignat_shiftr m wa n)
      (\m -> Native.bignat_shiftr m wa n)

bignat_shiftr_neg
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
bignat_shiftr_neg mwa wa n
   = mwaCompareOp mwa
      (\m -> Other.bignat_shiftr_neg m wa n)
      (\m -> Native.bignat_shiftr_neg m wa n)

bignat_or
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_or mwa wa wb
   = mwaCompareOp mwa
      (\m -> Other.bignat_or m wa wb)
      (\m -> Native.bignat_or m wa wb)

bignat_xor
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_xor mwa wa wb
   = mwaCompareOp mwa
      (\m -> Other.bignat_xor m wa wb)
      (\m -> Native.bignat_xor m wa wb)

bignat_and
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_and mwa wa wb
   = mwaCompareOp mwa
      (\m -> Other.bignat_and m wa wb)
      (\m -> Native.bignat_and m wa wb)

bignat_and_not
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_and_not mwa wa wb
   = mwaCompareOp mwa
      (\m -> Other.bignat_and_not m wa wb)
      (\m -> Native.bignat_and_not m wa wb)

bignat_quotrem
   :: MutableWordArray# RealWorld
   -> MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_quotrem mwq mwr wa wb
   = mwaCompareOp2 mwq mwr
      (\m1 m2 -> Other.bignat_quotrem m1 m2 wa wb)
      (\m1 m2 -> Native.bignat_quotrem m1 m2 wa wb)

bignat_quot
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_quot mwq wa wb
   = mwaCompareOp mwq
      (\m -> Other.bignat_quot m wa wb)
      (\m -> Native.bignat_quot m wa wb)

bignat_rem
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_rem mwr wa wb
   = mwaCompareOp mwr
      (\m -> Other.bignat_rem m wa wb)
      (\m -> Native.bignat_rem m wa wb)

bignat_quotrem_word
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> (# State# RealWorld, Word# #)
bignat_quotrem_word mwq wa b
   = mwaCompareOpWord mwq
      (\m -> Other.bignat_quotrem_word m wa b)
      (\m -> Native.bignat_quotrem_word m wa b)

bignat_quot_word
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
bignat_quot_word mwq wa b
   = mwaCompareOp mwq
      (\m -> Other.bignat_quot_word m wa b)
      (\m -> Native.bignat_quot_word m wa b)

bignat_rem_word
   :: WordArray#
   -> Word#
   -> Word#
bignat_rem_word wa b =
   let
      gr = Other.bignat_rem_word wa b
      nr = Native.bignat_rem_word wa b
   in case gr `eqWord#` nr of
       1# -> gr
       _  -> unexpectedValue_Word# (# #)

bignat_gcd
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_gcd mwr wa wb
   = mwaCompareOp mwr
      (\m -> Other.bignat_gcd m wa wb)
      (\m -> Native.bignat_gcd m wa wb)

bignat_gcd_word
   :: WordArray#
   -> Word#
   -> Word#
bignat_gcd_word wa b =
   let
      gr = Other.bignat_gcd_word wa b
      nr = Native.bignat_gcd_word wa b
   in case gr `eqWord#` nr of
       1# -> gr
       _  -> unexpectedValue_Word# (# #)

bignat_gcd_word_word
   :: Word#
   -> Word#
   -> Word#
bignat_gcd_word_word a b =
   let
      gr = Other.bignat_gcd_word_word a b
      nr = Native.bignat_gcd_word_word a b
   in case gr `eqWord#` nr of
       1# -> gr
       _  -> unexpectedValue_Word# (# #)

bignat_encode_double :: WordArray# -> Int# -> Double#
bignat_encode_double a e =
   let
      gr = Other.bignat_encode_double a e
      nr = Native.bignat_encode_double a e
   in case gr ==## nr of
       1# -> gr
       _  -> case unexpectedValue of
               !_ -> 0.0##
               -- see Note [ghc-bignum exceptions] in GHC.Num.Primitives

bignat_powmod_word :: WordArray# -> WordArray# -> Word# -> Word#
bignat_powmod_word b e m =
   let
      gr = Other.bignat_powmod_word b e m
      nr = Native.bignat_powmod_word b e m
   in case gr `eqWord#` nr of
       1# -> gr
       _  -> unexpectedValue_Word# (# #)

bignat_powmod
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_powmod r b e m
   = mwaCompareOp r
      (\r' -> Other.bignat_powmod r' b e m)
      (\r' -> Native.bignat_powmod r' b e m)

bignat_powmod_words
   :: Word#
   -> Word#
   -> Word#
   -> Word#
bignat_powmod_words b e m =
   let
      gr = Other.bignat_powmod_words b e m
      nr = Native.bignat_powmod_words b e m
   in case gr `eqWord#` nr of
       1# -> gr
       _  -> unexpectedValue_Word# (# #)

integer_gcde
   :: Integer
   -> Integer
   -> (# Integer, Integer, Integer #)
integer_gcde a b =
   let
      !(# g0,x0,y0 #) = Other.integer_gcde a b
      !(# g1,x1,y1 #) = Native.integer_gcde a b
   in if isTrue# (integerEq# x0 x1
                  &&# integerEq# y0 y1
                  &&# integerEq# g0 g1)
         then (# g0, x0, y0 #)
         else case unexpectedValue of
            !_ -> (# integerZero, integerZero, integerZero #)

integer_recip_mod
   :: Integer
   -> Natural
   -> (# Natural | () #)
integer_recip_mod x m =
   let
      !r0 = Other.integer_recip_mod x m
      !r1 = Native.integer_recip_mod x m
   in case (# r0, r1 #) of
         (# (# | () #), (# | () #) #) -> r0
         (# (# y0 | #), (# y1 | #) #)
            | isTrue# (naturalEq# y0 y1) -> r0
         _ -> case unexpectedValue of
            !_ -> (# | () #)

integer_powmod
   :: Integer
   -> Natural
   -> Natural
   -> Natural
integer_powmod b e m =
   let
      !r0 = Other.integer_powmod b e m
      !r1 = Native.integer_powmod b e m
   in if isTrue# (naturalEq# r0 r1)
         then r0
         else case unexpectedValue of
               !_ -> naturalZero
