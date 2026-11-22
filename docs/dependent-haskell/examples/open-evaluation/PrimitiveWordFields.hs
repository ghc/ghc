{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, MagicHash, TypeFamilies #-}
#include "../../../../libraries/ghc-internal/include/WordSize.h"
module PrimitiveWordFields (WordFieldResult, ZeroWord, MaximumWord) where

import Data.Kind (Type)
import GHC.Exts (Word(W#))

-- | Select a result type at each endpoint of the target word range.
-- Use the unticked spelling so CPP can expand WORD_MAXBOUND.
type family WordFieldResult (word :: Word) :: Type where
    WordFieldResult ('W# 0##) = Bool
    WordFieldResult (W# WORD_MAXBOUND##) = Char

-- | The zero endpoint of the target word range.
type ZeroWord = 'W# 0##

-- | The maximum endpoint of the target word range.
type MaximumWord = W# WORD_MAXBOUND##
