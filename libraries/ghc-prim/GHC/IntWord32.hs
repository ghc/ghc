{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IntWord32
-- Copyright   :  (c) The University of Glasgow, 1997-2008
-- License     :  see libraries/ghc-prim/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Primitive operations on Int32# and Word32# on platforms where
-- WORD_SIZE_IN_BITS < 32.
--
-----------------------------------------------------------------------------

#include "MachDeps.h"

-- #hide
module GHC.IntWord32 (
#if WORD_SIZE_IN_BITS < 32
    Int32#, Word32#, module GHC.IntWord32
#endif
 ) where

import GHC.Bool
import GHC.Prim

#if WORD_SIZE_IN_BITS < 32

foreign import unsafe "stg_eqWord32"      eqWord32#      :: Word32# -> Word32# -> Bool
foreign import unsafe "stg_neWord32"      neWord32#      :: Word32# -> Word32# -> Bool
foreign import unsafe "stg_ltWord32"      ltWord32#      :: Word32# -> Word32# -> Bool
foreign import unsafe "stg_leWord32"      leWord32#      :: Word32# -> Word32# -> Bool
foreign import unsafe "stg_gtWord32"      gtWord32#      :: Word32# -> Word32# -> Bool
foreign import unsafe "stg_geWord32"      geWord32#      :: Word32# -> Word32# -> Bool

foreign import unsafe "stg_eqInt32"       eqInt32#       :: Int32# -> Int32# -> Bool
foreign import unsafe "stg_neInt32"       neInt32#       :: Int32# -> Int32# -> Bool
foreign import unsafe "stg_ltInt32"       ltInt32#       :: Int32# -> Int32# -> Bool
foreign import unsafe "stg_leInt32"       leInt32#       :: Int32# -> Int32# -> Bool
foreign import unsafe "stg_gtInt32"       gtInt32#       :: Int32# -> Int32# -> Bool
foreign import unsafe "stg_geInt32"       geInt32#       :: Int32# -> Int32# -> Bool

foreign import unsafe "stg_int32ToWord32" int32ToWord32# :: Int32# -> Word32#
foreign import unsafe "stg_word32ToInt32" word32ToInt32# :: Word32# -> Int32#
foreign import unsafe "stg_intToInt32"    intToInt32#    :: Int# -> Int32#
foreign import unsafe "stg_wordToWord32"  wordToWord32#  :: Word# -> Word32#
foreign import unsafe "stg_word32ToWord"  word32ToWord#  :: Word32# -> Word#

foreign import unsafe "stg_plusInt32"     plusInt32#     :: Int32# -> Int32# -> Int32#
foreign import unsafe "stg_minusInt32"    minusInt32#    :: Int32# -> Int32# -> Int32#
foreign import unsafe "stg_timesInt32"    timesInt32#    :: Int32# -> Int32# -> Int32#
foreign import unsafe "stg_negateInt32"   negateInt32#   :: Int32# -> Int32#
foreign import unsafe "stg_quotInt32"     quotInt32#     :: Int32# -> Int32# -> Int32#
foreign import unsafe "stg_remInt32"      remInt32#      :: Int32# -> Int32# -> Int32#
foreign import unsafe "stg_quotWord32"    quotWord32#    :: Word32# -> Word32# -> Word32#
foreign import unsafe "stg_remWord32"     remWord32#     :: Word32# -> Word32# -> Word32#

foreign import unsafe "stg_and32"         and32#         :: Word32# -> Word32# -> Word32#
foreign import unsafe "stg_or32"          or32#          :: Word32# -> Word32# -> Word32#
foreign import unsafe "stg_xor32"         xor32#         :: Word32# -> Word32# -> Word32#
foreign import unsafe "stg_not32"         not32#         :: Word32# -> Word32#

foreign import unsafe "stg_iShiftL32"     iShiftL32#     :: Int32# -> Int# -> Int32#
foreign import unsafe "stg_iShiftRA32"    iShiftRA32#    :: Int32# -> Int# -> Int32#
foreign import unsafe "stg_shiftL32"      shiftL32#      :: Word32# -> Int# -> Word32#
foreign import unsafe "stg_shiftRL32"     shiftRL32#     :: Word32# -> Int# -> Word32#

#endif

