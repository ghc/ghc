{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[InstEnv]{Utilities for typechecking instance declarations}

The bits common to GHC.Tc.TyCl.Instance and GHC.Tc.Deriv.
-}

{-# LANGUAGE CPP, DeriveDataTypeable #-}

module GHC.Core.TyCon.Env (
        TyConEnv
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
    ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Tc.Utils.TcType -- InstEnv is really part of the type checker,
              -- and depends on TcType in many ways
import GHC.Core ( IsOrphan(..), isOrphan, chooseOrphanAnchor )
import GHC.Unit.Module.Env
import GHC.Unit.Types
import GHC.Core.Class
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Unique (getUnique)
import GHC.Core.Unify
import GHC.Types.Basic
import GHC.Types.Id
import Data.Data        ( Data )
import Data.Maybe       ( isJust, isNothing )

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Error
import GHC.Utils.Panic


import GHC.Types.Unique.FM
import GHC.Core.TyCon (TyCon)

type TyConEnv a = UniqFM TyCon a
