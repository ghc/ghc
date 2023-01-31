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
import Data.Coerce

-- | Information to be exposed in interface files which is produced
-- by the stg2stg passes.
type StgCgInfos = NameEnv TagSig

newtype TagSig  -- The signature for each binding, this is a newtype as we might
                -- want to track more information in the future.
  = TagSig TagInfo
  deriving (Eq)

data TagInfo
  = TagDunno            -- We don't know anything about the tag.
  | TagTuple [TagInfo]  -- Represents a function/thunk which when evaluated
                        -- will return a Unboxed tuple whos components have
                        -- the given TagInfos.
  | TagProper           -- Heap pointer to properly-tagged value
  | TagTagged           -- Bottom of the domain.
  deriving (Eq)

instance Outputable TagInfo where
  ppr TagTagged      = text "TagTagged"
  ppr TagDunno       = text "TagDunno"
  ppr TagProper      = text "TagProper"
  ppr (TagTuple tis) = text "TagTuple" <> brackets (pprWithCommas ppr tis)

instance Binary TagInfo where
  put_ bh TagDunno  = putByte bh 1
  put_ bh (TagTuple flds) = putByte bh 2 >> put_ bh flds
  put_ bh TagProper = putByte bh 3
  put_ bh TagTagged = putByte bh 4

  get bh = do tag <- getByte bh
              case tag of 1 -> return TagDunno
                          2 -> TagTuple <$> get bh
                          3 -> return TagProper
                          4 -> return TagTagged
                          _ -> panic ("get TagInfo " ++ show tag)

instance Outputable TagSig where
  ppr (TagSig ti) = char '<' <> ppr ti <> char '>'
instance OutputableBndr (Id,TagSig) where
  pprInfixOcc  = ppr
  pprPrefixOcc = ppr

instance Binary TagSig where
  put_ bh (TagSig sig) = put_ bh sig
  get bh = pure TagSig <*> get bh

isTaggedSig :: TagSig -> Bool
isTaggedSig (TagSig TagProper) = True
isTaggedSig (TagSig TagTagged) = True
isTaggedSig _ = False

seqTagSig :: TagSig -> ()
seqTagSig = coerce seqTagInfo

seqTagInfo :: TagInfo -> ()
seqTagInfo TagTagged      = ()
seqTagInfo TagDunno       = ()
seqTagInfo TagProper      = ()
seqTagInfo (TagTuple tis) = foldl' (\_unit sig -> seqTagSig (coerce sig)) () tis
