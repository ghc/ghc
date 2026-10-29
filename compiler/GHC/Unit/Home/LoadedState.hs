-- | State about home-unit modules whose interfaces were demand-loaded from disk.
--
-- See Note [Demand-loaded home interfaces]
module GHC.Unit.Home.LoadedState
  ( HomeLoadedState(..)
  , emptyHomeLoadedState
  ) where

import GHC.Prelude

import GHC.Core.FamInstEnv
import GHC.Core.InstEnv ( InstEnv, emptyInstEnv )
import GHC.Core.Rules ( RuleBase, emptyRuleBase )

import GHC.Linker.Types ( Linkable )

import GHC.Types.Annotations ( AnnEnv, emptyAnnEnv )
import GHC.Types.CompleteMatch
import GHC.Types.TypeEnv

import GHC.Unit.Module.Env
import GHC.Unit.Types

{- Note [Demand-loaded home interfaces]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In one-shot mode (ghc -c) we do not have a module graph and nothing is in
the HPT up front.  Interfaces of home-unit modules are read from disk on
demand by 'GHC.Iface.Load.loadInterface', exactly like interfaces of external
packages are.  Conceptually, though, these modules belong to the home unit,
so they are added to the HPT (as a 'HomeModInfo') and not to the
'ExternalPackageState'.

The HPT aggregates such as 'hugInstancesBelow' or 'hugRulesBelow' are
snapshots taken at a single point in time, using the module graph.  That works
for --make mode, where every module below the current one is in the HPT
before we start compiling it.  In one-shot mode, however, a home interface can
be demand-loaded at any point: while typechecking, desugaring or even
simplifying.  The instances, rules, etc. of such a module must become visible
right away.

Hence each 'HomePackageTable' carries a 'HomeLoadedState', which accumulates
the instances, rules, annotations, ... of all home modules demand-loaded into
it.  The consumers that consult the 'ExternalPackageState' pools also consult
the 'HomeLoadedState' pools, so they see the same information as before.

Invariant: the 'HomeLoadedState' is only populated by demand-loading home
interfaces, which only happens in one-shot mode (see Note [Home module load
error] in GHC.Iface.Load).  In --make mode and GHCi it stays empty, so there is
no double-counting with the 'hug*Below' functions, which return nothing in
one-shot mode.  Conversely, in one-shot mode the HPT contains only
demand-loaded modules, so functions that take the instances of *all* HPT
modules ('hugAllInstances' in 'GHC.Tc.Module.runTcInteractive') must not be
combined with the 'HomeLoadedState' pools.
-}

-- | Instances, rules, ... of the home-unit modules whose interfaces were
-- demand-loaded into the HPT.
--
-- See Note [Demand-loaded home interfaces]
data HomeLoadedState = HomeLoadedState
  { hls_is_boot :: !(InstalledModuleEnv ModuleNameWithIsBoot)
      -- ^ In one-shot mode (only), home-unit modules are demand-loaded,
      -- so we need to record which of them are boot modules.  We set this
      -- field after loading all the explicitly-imported interfaces, but
      -- before doing anything else.
      --
      -- The 'ModuleName' part is not necessary, but it's useful for
      -- debug prints, and it's convenient because this field comes
      -- direct from 'GHC.Tc.Utils.imp_dep_mods'

  , hls_inst_env :: !InstEnv
      -- ^ The total 'InstEnv' accumulated from all demand-loaded home modules

  , hls_fam_inst_env :: !FamInstEnv
      -- ^ The total 'FamInstEnv' accumulated from all demand-loaded home modules

  , hls_rule_base :: !RuleBase
      -- ^ The total 'RuleBase' accumulated from all demand-loaded home modules

  , hls_ann_env :: !AnnEnv
      -- ^ The total 'AnnEnv' accumulated from all demand-loaded home modules

  , hls_complete_matches :: !CompleteMatches
      -- ^ The total 'CompleteMatches' accumulated from all demand-loaded
      -- home modules

  , hls_iface_bytecode :: !(ModuleEnv (IO Linkable))
      -- ^ If an interface was written with @-fwrite-if-simplified-core@, this
      -- will contain an IO action that compiles bytecode from core bindings.
      --
      -- See Note [Interface Files with Core Definitions]

  , hls_boot_types :: !TypeEnv
      -- ^ The declarations of the hi-boot interface of the module currently
      -- being compiled, if we had to load it.
      --
      -- See Note [Loading your own hi-boot file] in "GHC.Iface.Load"
  }

emptyHomeLoadedState :: HomeLoadedState
emptyHomeLoadedState = HomeLoadedState
  { hls_is_boot          = emptyInstalledModuleEnv
  , hls_inst_env         = emptyInstEnv
  , hls_fam_inst_env     = emptyFamInstEnv
  , hls_rule_base        = emptyRuleBase
  , hls_ann_env          = emptyAnnEnv
  , hls_complete_matches = []
  , hls_iface_bytecode   = emptyModuleEnv
  , hls_boot_types       = emptyTypeEnv
  }
