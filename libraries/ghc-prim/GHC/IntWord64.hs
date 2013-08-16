{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IntWord64
-- Copyright   :  (c) The University of Glasgow, 1997-2008
-- License     :  see libraries/ghc-prim/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Primitive operations on Int64# and Word64# on platforms where
-- WORD_SIZE_IN_BITS < 64.
--
-----------------------------------------------------------------------------

#include "MachDeps.h"

-- #hide
module GHC.IntWord64 (
#if WORD_SIZE_IN_BITS < 64
        Int64#, Word64#, module GHC.IntWord64
#endif
    ) where

#if WORD_SIZE_IN_BITS < 64
import GHC.Prim
import GHC.Types

eqWord64# :: Word64# -> Word64# -> Bool
eqWord64# a b = tagToEnum# (eqWord64## a b)
neWord64# :: Word64# -> Word64# -> Bool
neWord64# a b = tagToEnum# (neWord64## a b)
ltWord64# :: Word64# -> Word64# -> Bool
ltWord64# a b = tagToEnum# (ltWord64## a b)
leWord64# :: Word64# -> Word64# -> Bool
leWord64# a b = tagToEnum# (leWord64## a b)
gtWord64# :: Word64# -> Word64# -> Bool
gtWord64# a b = tagToEnum# (gtWord64## a b)
geWord64# :: Word64# -> Word64# -> Bool
geWord64# a b = tagToEnum# (geWord64## a b)

eqInt64# :: Int64# -> Int64# -> Bool
eqInt64# a b = tagToEnum# (eqInt64## a b)
neInt64# :: Int64# -> Int64# -> Bool
neInt64# a b = tagToEnum# (neInt64## a b)
ltInt64# :: Int64# -> Int64# -> Bool
ltInt64# a b = tagToEnum# (ltInt64## a b)
leInt64# :: Int64# -> Int64# -> Bool
leInt64# a b = tagToEnum# (leInt64## a b)
gtInt64# :: Int64# -> Int64# -> Bool
gtInt64# a b = tagToEnum# (gtInt64## a b)
geInt64# :: Int64# -> Int64# -> Bool
geInt64# a b = tagToEnum# (geInt64## a b)

foreign import ccall unsafe "hs_eqWord64"    eqWord64##     :: Word64# -> Word64# -> Int#
foreign import ccall unsafe "hs_neWord64"    neWord64##     :: Word64# -> Word64# -> Int#
foreign import ccall unsafe "hs_ltWord64"    ltWord64##     :: Word64# -> Word64# -> Int#
foreign import ccall unsafe "hs_leWord64"    leWord64##     :: Word64# -> Word64# -> Int#
foreign import ccall unsafe "hs_gtWord64"    gtWord64##     :: Word64# -> Word64# -> Int#
foreign import ccall unsafe "hs_geWord64"    geWord64##     :: Word64# -> Word64# -> Int#

foreign import ccall unsafe "hs_eqInt64"     eqInt64##      :: Int64# -> Int64# -> Int#
foreign import ccall unsafe "hs_neInt64"     neInt64##      :: Int64# -> Int64# -> Int#
foreign import ccall unsafe "hs_ltInt64"     ltInt64##      :: Int64# -> Int64# -> Int#
foreign import ccall unsafe "hs_leInt64"     leInt64##      :: Int64# -> Int64# -> Int#
foreign import ccall unsafe "hs_gtInt64"     gtInt64##      :: Int64# -> Int64# -> Int#
foreign import ccall unsafe "hs_geInt64"     geInt64##      :: Int64# -> Int64# -> Int#
foreign import ccall unsafe "hs_quotInt64"   quotInt64#     :: Int64# -> Int64# -> Int64#
foreign import ccall unsafe "hs_remInt64"    remInt64#      :: Int64# -> Int64# -> Int64#

foreign import ccall unsafe "hs_plusInt64"   plusInt64#     :: Int64# -> Int64# -> Int64#
foreign import ccall unsafe "hs_minusInt64"  minusInt64#    :: Int64# -> Int64# -> Int64#
foreign import ccall unsafe "hs_timesInt64"  timesInt64#    :: Int64# -> Int64# -> Int64#
foreign import ccall unsafe "hs_negateInt64" negateInt64#   :: Int64# -> Int64#
foreign import ccall unsafe "hs_quotWord64"  quotWord64#    :: Word64# -> Word64# -> Word64#
foreign import ccall unsafe "hs_remWord64"   remWord64#     :: Word64# -> Word64# -> Word64#

foreign import ccall unsafe "hs_and64"       and64#         :: Word64# -> Word64# -> Word64#
foreign import ccall unsafe "hs_or64"        or64#          :: Word64# -> Word64# -> Word64#
foreign import ccall unsafe "hs_xor64"       xor64#         :: Word64# -> Word64# -> Word64#
foreign import ccall unsafe "hs_not64"       not64#         :: Word64# -> Word64#

foreign import ccall unsafe "hs_uncheckedShiftL64"   uncheckedShiftL64#   :: Word64# -> Int# -> Word64#
foreign import ccall unsafe "hs_uncheckedShiftRL64"  uncheckedShiftRL64#  :: Word64# -> Int# -> Word64#
foreign import ccall unsafe "hs_uncheckedIShiftL64"  uncheckedIShiftL64#  :: Int64# -> Int# -> Int64#
foreign import ccall unsafe "hs_uncheckedIShiftRA64" uncheckedIShiftRA64# :: Int64# -> Int# -> Int64#
foreign import ccall unsafe "hs_uncheckedIShiftRL64" uncheckedIShiftRL64# :: Int64# -> Int# -> Int64#

foreign import ccall unsafe "hs_int64ToWord64"   int64ToWord64#   :: Int64# -> Word64#
foreign import ccall unsafe "hs_word64ToInt64"   word64ToInt64#   :: Word64# -> Int64#
foreign import ccall unsafe "hs_intToInt64"      intToInt64#      :: Int# -> Int64#
foreign import ccall unsafe "hs_int64ToInt"      int64ToInt#      :: Int64# -> Int#
foreign import ccall unsafe "hs_wordToWord64"    wordToWord64#    :: Word# -> Word64#
foreign import ccall unsafe "hs_word64ToWord"    word64ToWord#    :: Word64# -> Word#

#endif

