{-# LANGUAGE DeriveGeneric #-}
module Distribution.Backpack.FullUnitId (
    FullUnitId(..),
    FullDb,
    expandOpenUnitId,
    expandUnitId
) where

import Distribution.Backpack
import Distribution.Types.ComponentId
import Distribution.Compat.Prelude

-- Unlike OpenUnitId, which could direct to a UnitId.
data FullUnitId = FullUnitId ComponentId OpenModuleSubst
    deriving (Show, Generic)

type FullDb = DefUnitId -> FullUnitId

expandOpenUnitId :: FullDb -> OpenUnitId -> FullUnitId
expandOpenUnitId _db (IndefFullUnitId cid subst)
    = FullUnitId cid subst
expandOpenUnitId db (DefiniteUnitId uid)
    = expandUnitId db uid

expandUnitId :: FullDb -> DefUnitId -> FullUnitId
expandUnitId db uid = db uid
