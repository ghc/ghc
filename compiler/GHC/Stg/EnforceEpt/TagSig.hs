{-# LANGUAGE TypeFamilies, DataKinds, GADTs, FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}

-- We export this type from this module instead of GHC.Stg.EnforceEpt.Types
-- because it's used by more than the analysis itself. For example in interface
-- files where we record a tag signature for bindings.
-- By putting the sig into its own module we can avoid module loops.
module GHC.Stg.EnforceEpt.TagSig

where

import GHC.Prelude

import GHC.Types.Var
import GHC.Types.Name.Env( NameEnv )
import GHC.Utils.Outputable
import GHC.Utils.Binary
import GHC.Utils.Panic.Plain

-- | Information to be exposed in interface files which is produced
-- by the stg2stg passes.
type StgCgInfos = NameEnv TagSig

-- Note [TagSig and TagInfo]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- 'TagSig' describes a *binding*; 'TagInfo' describes a *runtime value*.
--
-- A binding denotes either a value or a function:
--
--   * 'TagVal' i   — the binding holds a value whose runtime shape is 'i'.
--                    'i' covers thunks (TagDunno), evaluated/tagged heap
--                    pointers (TagEPT), unboxed tuples (TagTuple), and
--                    bottoming computations (TagBottoming).
--
--   * 'TagFun' i   — the binding is a function or join point. A function
--                    closure is always EPT. 'i' describes what a
--                    *saturated call* returns.
--                    See Note [TagInfo of functions] in GHC.Stg.EnforceEpt.
--
-- This split keeps 'combineAltInfo' at the value level: case alternatives
-- combine 'TagInfo', not 'TagSig'. An alternative that returns a function
-- closure gets 'TagEPT' (closure pointer is tagged) with no tracked return
-- info — the return info only matters once the function is applied, and at
-- that point it's looked up from the binding's 'TagSig' via
-- 'lookupReturnInfo'.

-- | The signature attached to a binding.
data TagSig
  = TagVal TagInfo        -- ^ A value binding (thunk, constructor, etc.)
  | TagFun TagInfo        -- ^ A function/join-point binding; carries the
                          -- TagInfo of saturated-call return values.
                          -- See Note [TagInfo of functions].
  deriving (Eq)

-- Note [TagInfo lattice]
-- ~~~~~~~~~~~~~~~~~~~~~~
-- The TagInfo lattice describes what we know about whether a runtime value
-- is properly tagged (pointer tag bits set for heap pointers):
--
--   TagBottoming (bottom) ⊑ {TagEPT, TagTuple _} ⊑ TagDunno (top)
--
-- TagBottoming is the identity element for 'combineAltInfo': it is used as
-- the initial signature in fixpoint loops and for dead-end (bottoming)
-- computations, since their return value can be given any tag — they never
-- actually return.

-- | What we know about a runtime value.
data TagInfo
  = TagDunno            -- ^ We don't know anything about the tag.
  | TagTuple [TagInfo]  -- ^ An unboxed tuple with taginfo for each element.
  | TagEPT              -- ^ An evaluated and properly tagged value.
                        -- See Note [Evaluated and Properly Tagged].
  | TagBottoming        -- ^ Bottom of the domain.
                        -- See Note [Bottom functions are TagBottoming] in GHC.Stg.EnforceEpt.
  deriving (Eq)

instance Outputable TagInfo where
  ppr TagBottoming      = text "TagBottoming"
  ppr TagDunno          = text "TagDunno"
  ppr TagEPT            = text "TagEPT"
  ppr (TagTuple tis)    = text "TagTuple" <> brackets (pprWithCommas ppr tis)

instance Binary TagInfo where
  put_ bh TagDunno        = putByte bh 1
  put_ bh (TagTuple flds) = putByte bh 2 >> put_ bh flds
  put_ bh TagEPT          = putByte bh 3
  put_ bh TagBottoming    = putByte bh 4

  get bh = do tag <- getByte bh
              case tag of 1 -> return TagDunno
                          2 -> TagTuple <$> get bh
                          3 -> return TagEPT
                          4 -> return TagBottoming
                          _ -> panic ("get TagInfo " ++ show tag)

instance Outputable TagSig where
  ppr (TagVal ti) = char '<' <> text "TagVal" <> brackets (ppr ti) <> char '>'
  ppr (TagFun ti) = char '<' <> text "TagFun" <> brackets (ppr ti) <> char '>'

instance OutputableBndr (Id,TagSig) where
  pprInfixOcc  = ppr
  pprPrefixOcc = ppr

instance Binary TagSig where
  put_ bh (TagVal ti) = putByte bh 1 >> put_ bh ti
  put_ bh (TagFun ti) = putByte bh 2 >> put_ bh ti
  get bh = do tag <- getByte bh
              case tag of 1 -> TagVal <$> get bh
                          2 -> TagFun <$> get bh
                          _ -> panic ("get TagSig " ++ show tag)

-- | Is the given binding known to be properly tagged (or irrelevant, as for
-- unboxed values and bottoming computations)?
isTaggedSig :: TagSig -> Bool
isTaggedSig (TagFun _)  = True
isTaggedSig (TagVal ti) = isTaggedInfo ti

-- | Is the given value-level tag known to be properly tagged?
-- NB: unboxed tuples are *not* treated as tagged here; they are handled
-- specially by the rewriter (which considers them already evaluated).
isTaggedInfo :: TagInfo -> Bool
isTaggedInfo TagEPT       = True
isTaggedInfo TagBottoming = True
isTaggedInfo _            = False

seqTagSig :: TagSig -> ()
seqTagSig (TagVal ti) = seqTagInfo ti
seqTagSig (TagFun ti) = seqTagInfo ti

seqTagInfo :: TagInfo -> ()
seqTagInfo TagBottoming   = ()
seqTagInfo TagDunno       = ()
seqTagInfo TagEPT         = ()
seqTagInfo (TagTuple tis) = foldl' (\_unit info -> seqTagInfo info) () tis
