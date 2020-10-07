{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[InstEnv]{Utilities for typechecking instance declarations}

The bits common to GHC.Tc.TyCl.Instance and GHC.Tc.Deriv.
-}

{-# LANGUAGE CPP, DeriveDataTypeable #-}

module GHC.Core.TyCon.Set -- (
        -- TyConSet
        -- DFunId, InstMatch, ClsInstLookupResult,
        -- OverlapFlag(..), OverlapMode(..), setOverlapModeMaybe,
        -- ClsInst(..), DFunInstType, pprInstance, pprInstanceHdr, pprInstances,
        -- instanceHead, instanceSig, mkLocalInstance, mkImportedInstance,
        -- instanceDFunId, updateClsInstDFun, instanceRoughTcs,
        -- fuzzyClsInstCmp, orphNamesOfClsInst,

        -- InstEnvs(..), VisibleOrphanModules, InstEnv,
        -- emptyInstEnv, extendInstEnv,
        -- deleteFromInstEnv, deleteDFunFromInstEnv,
        -- identicalClsInstHead,
        -- extendInstEnvList, lookupUniqueInstEnv, lookupInstEnv, instEnvElts, instEnvClasses,
        -- memberInstEnv,
        -- instIsVisible,
        -- classInstances, instanceBindFun,
        -- instanceCantMatch, roughMatchTcs,
        -- isOverlappable, isOverlapping, isIncoherent
    -- )
    where

#include "HsVersions.h"

import GHC.Prelude

-- import GHC.Tc.Utils.TcType -- InstEnv is really part of the type checker,
--               -- and depends on TcType in many ways
-- import GHC.Core ( IsOrphan(..), isOrphan, chooseOrphanAnchor )
-- import GHC.Unit.Module.Env
-- import GHC.Unit.Types
-- import GHC.Core.Class
-- import GHC.Types.Var
-- import GHC.Types.Var.Set
-- import GHC.Types.Name
-- import GHC.Types.Name.Set
-- import GHC.Types.Unique (getUnique)
-- import GHC.Core.Unify
-- import GHC.Types.Basic
-- import GHC.Types.Id
-- import Data.Data        ( Data )
-- import Data.Maybe       ( isJust, isNothing )

-- import GHC.Utils.Misc
-- import GHC.Utils.Outputable
-- import GHC.Utils.Error
-- import GHC.Utils.Panic


import GHC.Types.Unique.Set
import GHC.Core.TyCon (TyCon)

type TyConSet = UniqSet TyCon

emptyTyConSet       :: TyConSet
unitTyConSet        :: TyCon -> TyConSet
extendTyConSetList   :: TyConSet -> [TyCon] -> TyConSet
extendTyConSet    :: TyConSet -> TyCon -> TyConSet
mkTyConSet          :: [TyCon] -> TyConSet
unionTyConSet      :: TyConSet -> TyConSet -> TyConSet
unionTyConSets  :: [TyConSet] -> TyConSet
minusTyConSet       :: TyConSet -> TyConSet -> TyConSet
elemTyConSet        :: TyCon -> TyConSet -> Bool
isEmptyTyConSet     :: TyConSet -> Bool
delFromTyConSet     :: TyConSet -> TyCon -> TyConSet
delListFromTyConSet :: TyConSet -> [TyCon] -> TyConSet
filterTyConSet      :: (TyCon -> Bool) -> TyConSet -> TyConSet
intersectTyConSet   :: TyConSet -> TyConSet -> TyConSet
intersectsTyConSet  :: TyConSet -> TyConSet -> Bool
-- ^ True if there is a non-empty intersection.
-- @s1 `intersectsTyConSet` s2@ doesn't compute @s2@ if @s1@ is empty

isEmptyTyConSet    = isEmptyUniqSet
emptyTyConSet      = emptyUniqSet
unitTyConSet       = unitUniqSet
mkTyConSet         = mkUniqSet
extendTyConSetList = addListToUniqSet
extendTyConSet     = addOneToUniqSet
unionTyConSet      = unionUniqSets
unionTyConSets     = unionManyUniqSets
minusTyConSet      = minusUniqSet
elemTyConSet       = elementOfUniqSet
delFromTyConSet    = delOneFromUniqSet
filterTyConSet     = filterUniqSet
intersectTyConSet  = intersectUniqSets



delListFromTyConSet set ns = foldl' delFromTyConSet set ns

intersectsTyConSet s1 s2 = not (isEmptyTyConSet (s1 `intersectTyConSet` s2))

nameSetAny :: (TyCon -> Bool) -> TyConSet -> Bool
nameSetAny = uniqSetAny

nameSetAll :: (TyCon -> Bool) -> TyConSet -> Bool
nameSetAll = uniqSetAll

-- -- | Get the elements of a TyConSet with some stable ordering.
-- -- This only works for TyCons that originate in the source code or have been
-- -- tidied.
-- -- See Note [Deterministic UniqFM] to learn about nondeterminism
-- nameSetElemsStable :: TyConSet -> [TyCon]
-- nameSetElemsStable ns =
--   sortBy stableTyConCmp $ nonDetEltsUniqSet ns
--   -- It's OK to use nonDetEltsUniqSet here because we immediately sort
--   -- with stableNameCmp