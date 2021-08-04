-- |
-- Module      : Basement.Compat.Primitive
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Basement.Compat.Primitive
    ( bool#
    , PinnedStatus(..), toPinnedStatus#
    , compatMkWeak#
    , compatIsByteArrayPinned#
    , compatIsMutableByteArrayPinned#
    , unsafeCoerce#
    , Word(..)
    ) where

import qualified Prelude
import           GHC.Exts
import           GHC.Prim
import           GHC.Word
import           GHC.IO

import           Basement.Compat.PrimTypes

--  GHC 9.0  | Base 4.15
--  GHC 8.8  | Base 4.13 4.14
--  GHC 8.6  | Base 4.12
--  GHC 8.4  | Base 4.11
--  GHC 8.2  | Base 4.10
--  GHC 8.0  | Base 4.9
--  GHC 7.10 | Base 4.8
--  GHC 7.8  | Base 4.7
--  GHC 7.6  | Base 4.6
--  GHC 7.4  | Base 4.5
--
--  More complete list:
--  https://wiki.haskell.org/Base_package

-- | Flag record whether a specific byte array is pinned or not
data PinnedStatus = Pinned | Unpinned
    deriving (Prelude.Eq)

toPinnedStatus# :: Pinned# -> PinnedStatus
toPinnedStatus# 0# = Unpinned
toPinnedStatus# _  = Pinned

-- | turn an Int# into a Bool
bool# :: Int# -> Prelude.Bool
bool# v = isTrue# v
{-# INLINE bool# #-}

-- | A mkWeak# version that keep working on 8.0
--
-- signature change in ghc-prim:
-- * 0.4: mkWeak# :: o -> b -> c                                             -> State# RealWorld -> (#State# RealWorld, Weak# b#)
-- * 0.5 :mkWeak# :: o -> b -> (State# RealWorld -> (#State# RealWorld, c#)) -> State# RealWorld -> (#State# RealWorld, Weak# b#)
--
compatMkWeak# :: o -> b -> Prelude.IO () -> State# RealWorld -> (#State# RealWorld, Weak# b #)
compatMkWeak# o b c s = mkWeak# o b (case c of { IO f -> f }) s
{-# INLINE compatMkWeak# #-}

#if __GLASGOW_HASKELL__ >= 802
compatIsByteArrayPinned# :: ByteArray# -> Pinned#
compatIsByteArrayPinned# ba = isByteArrayPinned# ba

compatIsMutableByteArrayPinned# :: MutableByteArray# s -> Pinned#
compatIsMutableByteArrayPinned# ba = isMutableByteArrayPinned# ba
#else
foreign import ccall unsafe "basement_is_bytearray_pinned"
    compatIsByteArrayPinned# :: ByteArray# -> Pinned#

foreign import ccall unsafe "basement_is_bytearray_pinned"
    compatIsMutableByteArrayPinned# :: MutableByteArray# s -> Pinned#
#endif
