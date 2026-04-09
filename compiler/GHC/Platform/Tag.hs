-- | Dynamic pointer tagging
--
-- See Note [Data constructor dynamic tags]
module GHC.Platform.Tag
  ( DynTag
  , tAG_MASK
  , mAX_PTR_TAG
  , isSmallFamily
  , toDynTag
  , fromDynTag
  ) where

import GHC.Prelude

import GHC.Platform
import GHC.Utils.Panic.Plain (assert)

import Data.Word (Word8)

-- | The tag on a pointer (from the dynamic-tagging paper).
-- Wraps a 'Word8' because pointer tags are non-negative and bounded by the
-- number of tag bits on the platform (2 or 3 bits in practice), so values
-- never exceed 7.  Use 'toDynTag' to construct, 'fromDynTag' to extract.
-- See Note [Data constructor dynamic tags].
newtype DynTag = DynTag Word8
  deriving (Eq, Ord, Show)

{- Note [Data constructor dynamic tags]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The family size of a data type (the number of constructors
or the arity of a function) can be either:
   * small, if the family size < 2**tag_bits
   * big, otherwise.

Small families can have the constructor tag in the tag bits.
Big families always use the tag values 1..mAX_PTR_TAG to represent
evaluatedness, the last one lumping together all overflowing ones.
We don't have very many tag bits: for example, we have 2 bits on
x86-32 and 3 bits on x86-64.

Also see Note [Tagging big families] in GHC.StgToCmm.Expr

The interpreter also needs to be updated if we change the
tagging strategy; see tagConstr in rts/Interpreter.c.
-}

-- | Word-sized bitmask of the tag bits (all tag bits set to 1).
-- Used in bitwise operations such as 'cmmTagMask' and 'cmmPointerMask'.
-- This is a 'TargetInt' because it participates in word-width arithmetic
-- on the target (see Note [TargetInt] in GHC.Platform).
tAG_MASK :: Platform -> TargetInt
tAG_MASK platform = (1 `shiftL` pc_TAG_BITS (platformConstants platform)) - 1

-- | Maximum pointer tag value; equivalently the number of tag bits set.
-- This is the 'DynTag' companion to 'tAG_MASK': small enough to fit in
-- 'Word8' since it equals tAG_MASK but is used as a tag number, not a mask.
mAX_PTR_TAG :: Platform -> DynTag
mAX_PTR_TAG platform = DynTag (fromIntegral (tAG_MASK platform))

-- | Narrow a host-side 'Int' to a 'DynTag', asserting that the value is
-- non-negative and does not exceed 'mAX_PTR_TAG' for the given platform.
toDynTag :: Platform -> Int -> DynTag
toDynTag platform n =
  assert (n >= 0 && n <= fromDynTag (mAX_PTR_TAG platform)) $
  DynTag (fromIntegral n)

-- | Unwrap a 'DynTag' to a host-side 'Int'.
-- Safe because 'DynTag' values are always small: at most 'mAX_PTR_TAG',
-- bounded by the number of tag bits on the platform (typically 2 or 3 bits,
-- so at most 7).
fromDynTag :: DynTag -> Int
fromDynTag (DynTag w) = fromIntegral w

-- | Is a data type family small enough that each constructor can get
-- its own pointer tag?
--
-- 'fam_size' is a host-side constructor count; compare against
-- 'mAX_PTR_TAG' via 'fromDynTag' to stay in 'Int' arithmetic.
isSmallFamily :: Platform -> Int -> Bool
isSmallFamily platform fam_size = fam_size <= fromDynTag (mAX_PTR_TAG platform)
