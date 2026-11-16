-- | The External Package State (EPS): what we have loaded from the interfaces
-- of modules in external units.
--
-- The 'ExternalPackageState' is an abstract handle which can only be queried
-- (and extended by 'GHC.Iface.Load.loadInterface').
-- See Note [Abstract External Package State] in "GHC.Unit.External.Store".
module GHC.Unit.External
   ( ExternalUnitCache (..)
   , initExternalUnitCache
   , eucEPS
   , ExternalPackageState
     -- * Keyed queries
   , eps_lookupTyThing
   , eps_lookupIface
   , eps_moduleTyThings
   , eps_lookupModFamInstEnv
   , eps_lookupDefaults
   , eps_lookupBytecode
   , eps_lookupKnownKeys
   , eps_lookupFreeHoles
     -- * Snapshot queries
   , eps_getInstEnv
   , eps_getFamInstEnv
   , eps_getRuleBase
   , eps_getAnnEnv
   , eps_getCompleteMatches
   , eps_getStats
     -- * Updates
   , EpsModule (..)
   , eps_addModule
   , eps_addFailed
   , eps_addBootDecls
   , eps_addKnownKeys
   , eps_addFreeHoles
   , eps_bumpDeclsOut
     -- * Statistics
   , EpsStats(..)
   , addEpsInStats
   , KnownKeyNameMaps
   , PackageTypeEnv
   , PackageInstEnv
   , PackageFamInstEnv
   , PackageRuleBase
   , PackageCompleteMatches
   , lookupExternalIface
   , newInMemoryEps
   , module GHC.Unit.External.Iface
   )
where

import GHC.Prelude

import GHC.Unit
import GHC.Unit.External.Iface
import GHC.Unit.External.Store
import GHC.Unit.External.InMemory ( newInMemoryEps )

import GHC.Core.FamInstEnv
import GHC.Core.InstEnv ( InstEnv )
import GHC.Core.Rules ( RuleBase )

import GHC.Types.CompleteMatch
import GHC.Types.TypeEnv


type PackageTypeEnv          = TypeEnv
type PackageRuleBase         = RuleBase
type PackageInstEnv          = InstEnv
type PackageFamInstEnv       = FamInstEnv
type PackageCompleteMatches  = CompleteMatches

-- | Look up the slim interface of a module loaded into the EPS.
-- See Note [The EPS does not keep ModIfaces].
lookupExternalIface :: ExternalPackageState -> Module -> IO (Maybe ExternalIface)
lookupExternalIface = eps_lookupIface

-- | Information about the currently loaded external packages.
-- This is mutable because packages will be demand-loaded during
-- a compilation run as required.
--
-- The implementation of the EPS is exchangeable.
-- See Note [Abstract External Package State].
newtype ExternalUnitCache = ExternalUnitCache
  { euc_eps :: ExternalPackageState
  }

-- | A fresh 'ExternalUnitCache' with GHC's in-memory EPS store.
initExternalUnitCache :: IO ExternalUnitCache
initExternalUnitCache = ExternalUnitCache <$> newInMemoryEps

eucEPS :: ExternalUnitCache -> ExternalPackageState
eucEPS = euc_eps
