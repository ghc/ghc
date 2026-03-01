-- | Dynamic pointer tagging
--
-- See Note [Data constructor dynamic tags]
module GHC.Platform.Tag
  ( DynTag
  , tAG_MASK
  , mAX_PTR_TAG
  , isSmallFamily
  ) where

import GHC.Prelude

import GHC.Platform

-- | The tag on a pointer (from the dynamic-tagging paper)
type DynTag = Int

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

-- | Tag bits mask / maximum pointer tag value, derived from the
-- number of tag bits on the platform.
tAG_MASK, mAX_PTR_TAG :: Platform -> Int
tAG_MASK platform = (1 `shiftL` pc_TAG_BITS (platformConstants platform)) - 1
mAX_PTR_TAG = tAG_MASK

-- | Is a data type family small enough that each constructor can get
-- its own pointer tag?
isSmallFamily :: Platform -> Int -> Bool
isSmallFamily platform fam_size = fam_size <= mAX_PTR_TAG platform
