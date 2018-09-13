{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module GHC.Driver.Env
   ( Hsc(..)
   , HscEnv (..)
   , runHsc
   , runHscWithHomeUnit
   , mkInteractiveHscEnv
   , runInteractiveHsc
   , hsc_HPT
   , hsc_dflags
   , hsc_home_unit
   , hsc_currentUnit
   , set_hsc_currentUnit
   -- * Functions to modify the current unit of the 'InternalUnitEnv'
   , set_hsc_dflags
   , modify_hsc_dflags
   , set_hsc_HPT
   , modify_hsc_HPT
   , set_hsc_home_unit
   , modify_hsc_home_unit
   -- * Modify arbitrary elements of the unit environment
   , hsc_findInternalUnitEnv_maybe
   , hsc_findInternalUnitEnv
   , hsc_memberInternalUnitEnv
   , hsc_unitHPT_maybe
   , hsc_unitHPT
   , set_hsc_unitHPT
   , modify_hsc_unitHPT
   , hsc_unitHomeUnit_maybe
   , hsc_unitHomeUnit
   , set_hsc_unitHomeUnit
   , modify_hsc_unitHomeUnit
   , hsc_unitDflags_maybe
   , hsc_unitDflags
   , set_hsc_unitDflags
   , modify_hsc_unitDflags
   -- * Internal Unit Env
   , InternalUnitEnv(..)
   -- * Get all elements of a unit environment
   , hsc_HPTs
   , hsc_allDflags
   , hsc_allHomeUnits
   , hsc_allUnitIds
   , hsc_internalUnitEnvs
   -- * Create and modify the unit environment
   , new_hsc_internalUnitEnv
   , singleton_hsc_unitEnv
   , insert_hsc_unitEnv
   , set_hsc_internalUnitEnvList
   , set_hsc_internalUnitEnv
   , set_hsc_internalUnitEnvGraph
   , modify_hsc_internalUnitenv
   -- * Dependency functionality for unit environment
   , hsc_currentHomeUnitDependencies
   , hsc_homeUnitDependencies
   , hsc_homeUnitDependencies_unitEnv
   , unitEnv_homeUnitDependencies
   , unitEnv_restrictHomeUnitDependencies
   -- * Pretty print internal unit environment
   , pprInternalUnitMap
   , pprUnitEnv
   , pprInternalUnitEnv

   -- * Unit Env type and functions
   , UnitEnvGraph(..)
   , UnitEnv
   , unitEnv_new
   , unitEnv_singleton
   , unitEnv_setCurrentUnit
   , unitEnv_insert
   , unitEnv_delete
   , unitEnv_adjust
   , unitEnv_setGraph
   , unitEnv_member
   , unitEnv_lookup_maybe
   , unitEnv_lookup

   , hscEPS
   , hptCompleteSigs
   , hptInstances
   , hptAnns
   , hptAllThings
   , hptSomeThingsBelowUs
   , hptRules
   , prepareAnnotations
   , lookupType
   , lookupIfaceByModule
   )
where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Driver.Ppr
import GHC.Driver.Session
import GHC.Unit.Finder.Types

import GHC.Runtime.Context
import GHC.Runtime.Interpreter.Types (Interp)
import GHC.Runtime.Linker.Types ( DynLinker )

import GHC.Unit
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.Deps
import GHC.Unit.Home.ModInfo
import GHC.Unit.External

import GHC.Core         ( CoreRule )
import GHC.Core.FamInstEnv
import GHC.Core.InstEnv ( ClsInst )

import GHC.Types.Annotations ( Annotation, AnnEnv, mkAnnEnv, plusAnnEnv )
import GHC.Types.CompleteMatch
import GHC.Types.Name
import GHC.Types.Name.Cache
import GHC.Types.Name.Env
import GHC.Types.Target
import GHC.Types.TypeEnv
import GHC.Types.TyThing

import GHC.Builtin.Names ( gHC_PRIM )

import GHC.Data.Maybe
import GHC.Data.Bag

import GHC.Unit.Module.Graph

import GHC.Utils.Outputable
import GHC.Utils.Monad
import GHC.Utils.Error
import GHC.Utils.Panic
import GHC.Utils.Misc

import Control.Monad    ( guard, ap )
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Unit.State (UnitState(explicitUnits))

-- | The Hsc monad: Passing an environment and warning state
newtype Hsc a = Hsc (HscEnv -> WarningMessages -> IO (a, WarningMessages))
    deriving (Functor)

instance Applicative Hsc where
    pure a = Hsc $ \_ w -> return (a, w)
    (<*>) = ap

instance Monad Hsc where
    Hsc m >>= k = Hsc $ \e w -> do (a, w1) <- m e w
                                   case k a of
                                       Hsc k' -> k' e w1

instance MonadIO Hsc where
    liftIO io = Hsc $ \_ w -> do a <- io; return (a, w)

instance HasDynFlags Hsc where
    getDynFlags = Hsc $ \e w -> return (hsc_dflags e, w)

runHsc :: HscEnv -> Hsc a -> IO a
runHsc hsc_env (Hsc hsc) = do
    (a, w) <- hsc hsc_env emptyBag
    printOrThrowWarnings (hsc_dflags hsc_env) w
    return a

runHscWithHomeUnit :: HscEnv -> UnitId -> Hsc a -> IO a
runHscWithHomeUnit hsc_env uid (Hsc hsc) = do
    let hsc_env' = set_hsc_currentUnit uid hsc_env
    (a, w) <- hsc hsc_env' emptyBag
    printOrThrowWarnings (hsc_dflags hsc_env') w
    return a

runInteractiveHsc :: HscEnv -> Hsc a -> IO a
-- A variant of runHsc that switches in the DynFlags from the
-- InteractiveContext before running the Hsc computation.
runInteractiveHsc hsc_env action = do
  runHsc (mkInteractiveHscEnv hsc_env) action


mkInteractiveHscEnv :: HscEnv -> HscEnv
mkInteractiveHscEnv hsc_env = set_hsc_currentInternalUnitEnv hsc_env interactiveInternalUnitEnv
  where
  interactiveInternalUnitEnv = InternalUnitEnv
      { internalUnitEnv_dflags = ic_dflags (hsc_IC hsc_env)
      , internalUnitEnv_homePackageTable = hsc_HPT hsc_env
      , internalUnitEnv_home_unit = mkHomeUnitFromFlags (ic_dflags (hsc_IC hsc_env))
      }

-- | HscEnv is like 'GHC.Driver.Monad.Session', except that some of the fields are immutable.
-- An HscEnv is used to compile a single module from plain Haskell source
-- code (after preprocessing) to either C, assembly or C--. It's also used
-- to store the dynamic linker state to allow for multiple linkers in the
-- same address space.
-- Things like the module graph don't change during a single compilation.
--
-- Historical note: \"hsc\" used to be the name of the compiler binary,
-- when there was a separate driver and compiler.  To compile a single
-- module, the driver would invoke hsc on the source code... so nowadays
-- we think of hsc as the layer of the compiler that deals with compiling
-- a single module.
data HscEnv
  = HscEnv {
        hsc_internalUnitEnv :: UnitEnv,
                -- ^ Information per package / unit, for "internal" (not already
                -- installed) units.


        hsc_targets :: [Target],
                -- ^ The targets (or roots) of the current session

        hsc_mod_graph :: ModuleGraph,
                -- ^ The module graph of the current session

        hsc_IC :: InteractiveContext,
                -- ^ The context for evaluating interactive statements

        hsc_EPS :: {-# UNPACK #-} !(IORef ExternalPackageState),
                -- ^ Information about the currently loaded external packages.
                -- This is mutable because packages will be demand-loaded during
                -- a compilation run as required.

        hsc_NC  :: {-# UNPACK #-} !(IORef NameCache),
                -- ^ As with 'hsc_EPS', this is side-effected by compiling to
                -- reflect sucking in interface files.  They cache the state of
                -- external interface files, in effect.

        hsc_FC   :: {-# UNPACK #-} !(IORef FinderCache),
                -- ^ The cached result of performing finding in the file system

        hsc_type_env_var :: Maybe (Module, IORef TypeEnv)
                -- ^ Used for one-shot compilation only, to initialise
                -- the 'IfGblEnv'. See 'GHC.Tc.Utils.tcg_type_env_var' for
                -- 'GHC.Tc.Utils.TcGblEnv'.  See also Note [hsc_type_env_var hack]

        , hsc_interp :: Maybe Interp
                -- ^ target code interpreter (if any) to use for TH and GHCi.
                -- See Note [Target code interpreter]

        , hsc_dynLinker :: DynLinker
                -- ^ dynamic linker.

 }


data InternalUnitEnv = InternalUnitEnv
  { internalUnitEnv_dflags :: DynFlags
    -- ^ The dynamic flag settings
  , internalUnitEnv_homePackageTable :: HomePackageTable
    -- ^ The home package table describes already-compiled
    -- home-package modules, /excluding/ the module we
    -- are compiling right now.
    -- (In one-shot mode the current module is the only
    -- home-package module, so hsc_HPT is empty.  All other
    -- modules count as \"external-package\" modules.
    -- However, even in GHCi mode, hi-boot interfaces are
    -- demand-loaded into the external-package table.)
    --
    -- 'hsc_HPT' is not mutable because we only demand-load
    -- external packages; the home package is eagerly
    -- loaded, module by module, by the compilation manager.
    --
    -- The HPT may contain modules compiled earlier by @--make@
    -- but not actually below the current module in the dependency
    -- graph.
    --
    -- (This changes a previous invariant: changed Jan 05.)
  , internalUnitEnv_home_unit :: HomeUnit
    -- ^ Home-unit
  }

-- -----------------------------------------------------------------------------

type UnitEnv = UnitEnvGraph InternalUnitEnv

data UnitEnvGraph v = UnitEnvGraph
  { unitEnv_graph :: !(Map UnitId v)
  , unitEnv_currentUnit :: !UnitId
  } deriving (Functor, Foldable, Traversable)

unitEnv_setCurrentUnit :: UnitId -> UnitEnvGraph v -> UnitEnvGraph v
unitEnv_setCurrentUnit u env = env { unitEnv_currentUnit = u }

unitEnv_insert :: UnitId -> v -> UnitEnvGraph v -> UnitEnvGraph v
unitEnv_insert unitId env unitEnv = unitEnv
  { unitEnv_graph = Map.insert unitId  env (unitEnv_graph unitEnv)
  }

unitEnv_delete :: UnitId -> UnitEnvGraph v -> UnitEnvGraph v
unitEnv_delete uid unitEnv = unitEnv
  { unitEnv_graph = Map.delete uid (unitEnv_graph unitEnv)
  }

unitEnv_adjust :: (v -> v) -> UnitId -> UnitEnvGraph v -> UnitEnvGraph v
unitEnv_adjust f uid unitEnv = unitEnv
  { unitEnv_graph = Map.adjust f uid (unitEnv_graph unitEnv)
  }

unitEnv_new :: UnitId -> Map UnitId v -> UnitEnvGraph v
unitEnv_new active m = UnitEnvGraph
  { unitEnv_graph = m
  , unitEnv_currentUnit = active
  }

unitEnv_singleton :: UnitId -> v -> UnitEnvGraph v
unitEnv_singleton active m = UnitEnvGraph
  { unitEnv_graph = Map.singleton active m
  , unitEnv_currentUnit = active
  }

unitEnv_setGraph :: Map UnitId v -> UnitEnvGraph v -> UnitEnvGraph v
unitEnv_setGraph m unitEnv = unitEnv
  { unitEnv_graph = m
  }

unitEnv_member :: UnitId -> UnitEnvGraph v -> Bool
unitEnv_member u env = Map.member u (unitEnv_graph env)

unitEnv_lookup_maybe :: UnitId -> UnitEnvGraph v -> Maybe v
unitEnv_lookup_maybe u env = Map.lookup u (unitEnv_graph env)

unitEnv_lookup :: UnitId -> UnitEnvGraph v -> v
unitEnv_lookup u env = fromJust $ unitEnv_lookup_maybe u env

-- -----------------------------------------------------------------------------
-- Asserts to enforce invariants for the UnitEnv.
-- -----------------------------------------------------------------------------

assertUnitEnvKnownUnit :: HasCallStack => UnitId -> UnitEnv -> UnitEnv
assertUnitEnvKnownUnit pkg e =
  ASSERT2(unitEnv_member pkg e, errMsg) e
  where
    errMsg = text "assertUnitKnown" <+> ppr (unitEnv_currentUnit e) $$ nest 2 (ppr (Map.keys $ unitEnv_graph e))

assertUnitEnvInvariant :: HasCallStack => UnitEnv -> UnitEnv
assertUnitEnvInvariant e = assertUnitEnvKnownUnit (unitEnv_currentUnit e) $ assertUnitEnvConsistent e

assertUnitEnvConsistent :: HasCallStack => UnitEnv -> UnitEnv
assertUnitEnvConsistent env =
  ASSERT2(all sameUnitId entries, inconsistentDflagsMsg) env
  where
    entries = Map.assocs $ unitEnv_graph env

    sameUnitId (uid, env) = homeUnitId (internalUnitEnv_home_unit env) == uid

    inconsistentDflagsMsg =
      text "Unit Environment is inconsistent. An Entry has a different unit than its dflags homeUnitId:"
      $$ nest 2
          ( vcat
            $ map
                (\(uid, e) ->
                  ppr uid <+> text "->" <+> ppr (homeUnitId_ $ internalUnitEnv_dflags e)
                )
                entries
          )

assertHscEnvInvariant :: HasCallStack => HscEnv -> HscEnv
assertHscEnvInvariant e = e { hsc_internalUnitEnv = assertUnitEnvInvariant $ hsc_internalUnitEnv e }

-- -----------------------------------------------------------------------------
-- Pretty output functions
-- -----------------------------------------------------------------------------

pprInternalUnitMap :: HasCallStack => HscEnv -> SDoc
pprInternalUnitMap env = text "pprInternalUnitMap"
  $$ nest 2 (pprUnitEnv $ hsc_internalUnitEnv env)

pprUnitEnv :: HasCallStack => UnitEnv -> SDoc
pprUnitEnv unitEnv = text "Active unit:" <+> ppr (unitEnv_currentUnit unitEnv) $$ vcat (map (\(k, v) -> pprInternalUnitEnv k v) $ Map.assocs $ unitEnv_graph unitEnv)

pprInternalUnitEnv :: HasCallStack => UnitId -> InternalUnitEnv -> SDoc
pprInternalUnitEnv uid env = ppr uid <+> text "->" $$ nest 4 (pprHPT $ internalUnitEnv_homePackageTable env)

-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Functions to access the currently active home unit
-- -----------------------------------------------------------------------------

hsc_HPT :: HasCallStack => HscEnv -> HomePackageTable
hsc_HPT = internalUnitEnv_homePackageTable . hsc_currentInternalUnitEnv

hsc_dflags :: HasCallStack => HscEnv -> DynFlags
hsc_dflags = internalUnitEnv_dflags . hsc_currentInternalUnitEnv

hsc_home_unit :: HasCallStack => HscEnv -> HomeUnit
hsc_home_unit = internalUnitEnv_home_unit . hsc_currentInternalUnitEnv

set_hsc_dflags :: HasCallStack => DynFlags -> HscEnv -> HscEnv
set_hsc_dflags dflags hsc_env = set_hsc_unitDflags (hsc_currentUnit hsc_env) dflags hsc_env

modify_hsc_dflags :: HasCallStack => (DynFlags -> DynFlags) -> HscEnv -> HscEnv
modify_hsc_dflags f e = modify_hsc_unitDflags f (hsc_currentUnit e) e

set_hsc_unitDflags :: HasCallStack => UnitId -> DynFlags -> HscEnv -> HscEnv
set_hsc_unitDflags uid dflags e =
  modify_hsc_unitDflags (const dflags) uid e

modify_hsc_unitDflags :: HasCallStack => (DynFlags -> DynFlags) -> UnitId -> HscEnv -> HscEnv
modify_hsc_unitDflags f uid e = modify_hsc_internalUnitenv update uid (assertHscEnvInvariant e)
  where
    update unitEnv = unitEnv { internalUnitEnv_dflags = f $ internalUnitEnv_dflags unitEnv }

set_hsc_home_unit :: HasCallStack => HomeUnit -> HscEnv -> HscEnv
set_hsc_home_unit homeUnit hsc_env = set_hsc_unitHomeUnit (hsc_currentUnit hsc_env) homeUnit hsc_env

modify_hsc_home_unit :: HasCallStack => (HomeUnit -> HomeUnit) -> HscEnv -> HscEnv
modify_hsc_home_unit f e = modify_hsc_unitHomeUnit f (hsc_currentUnit e) e

set_hsc_unitHomeUnit :: HasCallStack => UnitId -> HomeUnit -> HscEnv -> HscEnv
set_hsc_unitHomeUnit uid homeUnit e =
  modify_hsc_unitHomeUnit (const homeUnit) uid e

modify_hsc_unitHomeUnit :: HasCallStack => (HomeUnit -> HomeUnit) -> UnitId -> HscEnv -> HscEnv
modify_hsc_unitHomeUnit f uid e = modify_hsc_internalUnitenv update uid (assertHscEnvInvariant e)
  where
    update unitEnv = unitEnv { internalUnitEnv_home_unit = f $ internalUnitEnv_home_unit unitEnv }

set_hsc_HPT :: HasCallStack => HomePackageTable -> HscEnv -> HscEnv
set_hsc_HPT hpt hsc_env = modify_hsc_HPT (const hpt) hsc_env

set_hsc_unitHPT :: HasCallStack => UnitId -> HomePackageTable -> HscEnv -> HscEnv
set_hsc_unitHPT uid hpt e = modify_hsc_unitHPT (const hpt) uid e

modify_hsc_unitHPT :: HasCallStack => (HomePackageTable -> HomePackageTable) -> UnitId -> HscEnv -> HscEnv
modify_hsc_unitHPT f uid hsc_env = modify_hsc_internalUnitenv update uid hsc_env
  where
    update unitEnv = unitEnv { internalUnitEnv_homePackageTable = f $ internalUnitEnv_homePackageTable unitEnv }

modify_hsc_HPT :: HasCallStack => (HomePackageTable -> HomePackageTable) -> HscEnv -> HscEnv
modify_hsc_HPT f e = modify_hsc_unitHPT f (hsc_currentUnit e) e

-- -----------------------------------------------------------------------------

hsc_currentInternalUnitEnv :: HasCallStack => HscEnv -> InternalUnitEnv
hsc_currentInternalUnitEnv e =
  case hsc_findInternalUnitEnv_maybe (hsc_currentUnit e) $ assertHscEnvInvariant e of
    Just unitEnv -> unitEnv
    Nothing -> pprPanic "packageNotFound" $
      ppr $ hsc_currentUnit e

set_hsc_currentUnit :: HasCallStack => UnitId -> HscEnv -> HscEnv
set_hsc_currentUnit u hsc_env = hsc_env
  { hsc_internalUnitEnv = unitEnv_setCurrentUnit u (hsc_internalUnitEnv hsc_env)
  }

-- -----------------------------------------------------------------------------
-- Operations on arbitrary elements of the home unit graph
-- -----------------------------------------------------------------------------

hsc_findInternalUnitEnv_maybe :: HasCallStack => UnitId -> HscEnv -> Maybe InternalUnitEnv
hsc_findInternalUnitEnv_maybe uid e =
  unitEnv_lookup_maybe uid (hsc_internalUnitEnv $ assertHscEnvInvariant e)

hsc_findInternalUnitEnv :: HasCallStack => UnitId -> HscEnv -> InternalUnitEnv
hsc_findInternalUnitEnv uid e =
  unitEnv_lookup uid (hsc_internalUnitEnv $ assertHscEnvInvariant e)

hsc_memberInternalUnitEnv :: HasCallStack => UnitId -> HscEnv -> Bool
hsc_memberInternalUnitEnv uid e =
  unitEnv_member uid (hsc_internalUnitEnv $ assertHscEnvInvariant e)

hsc_unitHPT_maybe :: HasCallStack => UnitId -> HscEnv -> Maybe HomePackageTable
hsc_unitHPT_maybe uid hsc_env =
  fmap internalUnitEnv_homePackageTable (hsc_findInternalUnitEnv_maybe uid hsc_env)

hsc_unitHPT :: HasCallStack => UnitId -> HscEnv -> HomePackageTable
hsc_unitHPT uid hsc_env = case hsc_unitHPT_maybe uid hsc_env of
    Nothing -> pprPanic "Unit unknown to the internal unit environment"
        $ text "unit (" <> ppr uid <> text ")"
          $$ pprInternalUnitMap hsc_env
    Just hpt -> hpt

hsc_unitHomeUnit_maybe :: HasCallStack => UnitId -> HscEnv -> Maybe HomeUnit
hsc_unitHomeUnit_maybe uid hsc_env =
  fmap internalUnitEnv_home_unit (hsc_findInternalUnitEnv_maybe uid hsc_env)

hsc_unitHomeUnit :: HasCallStack => UnitId -> HscEnv -> HomeUnit
hsc_unitHomeUnit uid hsc_env = case hsc_unitHomeUnit_maybe uid hsc_env of
    Nothing -> pprPanic "Unit unknown to the internal unit environment"
        $ text "unit (" <> ppr uid <> text ")"
          $$ pprInternalUnitMap hsc_env
    Just homeUnit -> homeUnit

hsc_unitDflags_maybe ::HasCallStack => UnitId -> HscEnv -> Maybe DynFlags
hsc_unitDflags_maybe uid e =
  fmap internalUnitEnv_dflags (hsc_findInternalUnitEnv_maybe uid e)

hsc_unitDflags :: HasCallStack => UnitId -> HscEnv -> DynFlags
hsc_unitDflags uid e = case hsc_unitDflags_maybe uid e of
    Nothing -> pprPanic "Unit unknown to the internal unit environment"
        $ text "unit (" <> ppr uid <> text ")"
          $$ pprInternalUnitMap e

    Just dflags -> dflags

new_hsc_internalUnitEnv :: HasCallStack => UnitId -> Map UnitId InternalUnitEnv -> HscEnv ->  HscEnv
new_hsc_internalUnitEnv unitId home_unit_env e = assertHscEnvInvariant e
  { hsc_internalUnitEnv = unitEnv_new unitId home_unit_env
  }

singleton_hsc_unitEnv :: HasCallStack => HscEnv -> UnitId -> InternalUnitEnv -> HscEnv
singleton_hsc_unitEnv hsc_env unitId unitEnv = assertHscEnvInvariant $ hsc_env
  { hsc_internalUnitEnv = unitEnv_new unitId (Map.singleton unitId unitEnv)
  }

insert_hsc_unitEnv :: HasCallStack => HscEnv -> InternalUnitEnv -> HscEnv
insert_hsc_unitEnv e internalUnitEnv = assertHscEnvInvariant $ e
  { hsc_internalUnitEnv = unitEnv_insert (homeUnitId_ $ internalUnitEnv_dflags internalUnitEnv) internalUnitEnv $ hsc_internalUnitEnv e
  }

set_hsc_internalUnitEnvList :: HasCallStack => [(UnitId, InternalUnitEnv)] -> HscEnv -> HscEnv
set_hsc_internalUnitEnvList home_unit_env hsc_env =
  set_hsc_internalUnitEnv (Map.fromList home_unit_env) hsc_env

set_hsc_internalUnitEnv :: HasCallStack => Map UnitId InternalUnitEnv -> HscEnv ->  HscEnv
set_hsc_internalUnitEnv home_unit_env e = assertHscEnvInvariant e
  { hsc_internalUnitEnv = unitEnv_new (hsc_currentUnit e) home_unit_env
  }

set_hsc_internalUnitEnvGraph :: HasCallStack => UnitEnv -> HscEnv ->  HscEnv
set_hsc_internalUnitEnvGraph unitEnv e = assertHscEnvInvariant e
  { hsc_internalUnitEnv = unitEnv
  }

modify_hsc_internalUnitenv :: HasCallStack => (InternalUnitEnv -> InternalUnitEnv) -> UnitId -> HscEnv -> HscEnv
modify_hsc_internalUnitenv f uid e = assertHscEnvInvariant e
  { hsc_internalUnitEnv = unitEnv_adjust f uid $ hsc_internalUnitEnv e
  }

-- | Rename a unit id in the internal unit env.
--
-- @'rename_hsc_unitId' oldUnit newUnit hscEnv@, it is assumed that the 'oldUnit' exists in the map,
-- otherwise we panic.
-- The 'DynFlags' associated with the home unit will have its field 'homeUnitId' set to 'newUnit'.
-- rename_hsc_unitId :: HasCallStack => UnitId -> UnitId -> HscEnv -> HscEnv
-- rename_hsc_unitId oldUnit newUnit hscEnv = case hsc_findInternalUnitEnv_maybe oldUnit hscEnv of
--   Nothing ->
--     pprPanic "Tried to rename unit, but it didn't exist"
--               $ text "Rename old unit \"" <> ppr oldUnit <> text "\" to \""<> ppr newUnit <> text "\""
--               $$ nest 2 (pprInternalUnitMap hscEnv)
--   Just oldEnv ->
--     let
--       activeUnit :: UnitId
--       !activeUnit = if hsc_currentUnit hscEnv == oldUnit
--                 then newUnit
--                 else hsc_currentUnit hscEnv

--       newInternalUnitEnv = oldEnv
--         { internalUnitEnv_dflags = (internalUnitEnv_dflags oldEnv)
--             { homeUnitId_ = newUnit
--             }
--         }
--     in
--     assertHscEnvInvariant $ hscEnv
--       { hsc_internalUnitEnv =
--           unitEnv_setCurrentUnit activeUnit
--           $ unitEnv_insert newUnit newInternalUnitEnv
--           $ unitEnv_delete oldUnit
--           $ hsc_internalUnitEnv hscEnv
--       }

-- -----------------------------------------------------------------------------
-- Getters
-- -----------------------------------------------------------------------------

hsc_HPTs :: HasCallStack => HscEnv -> [HomePackageTable]
hsc_HPTs = map internalUnitEnv_homePackageTable . hsc_internalUnitEnvs

hsc_internalUnitEnvs :: HasCallStack => HscEnv -> [InternalUnitEnv]
hsc_internalUnitEnvs = Map.elems . unitEnv_graph . hsc_internalUnitEnv . assertHscEnvInvariant

hsc_allDflags :: HasCallStack => HscEnv -> [DynFlags]
hsc_allDflags = map internalUnitEnv_dflags . hsc_internalUnitEnvs

hsc_allHomeUnits :: HasCallStack => HscEnv -> [HomeUnit]
hsc_allHomeUnits = map internalUnitEnv_home_unit . hsc_internalUnitEnvs

hsc_allUnitIds :: HasCallStack => HscEnv -> Set UnitId
hsc_allUnitIds = Map.keysSet . unitEnv_graph . hsc_internalUnitEnv . assertHscEnvInvariant

-- -----------------------------------------------------------------------------
-- Functionality for home units
-- -----------------------------------------------------------------------------

hsc_currentHomeUnitDependencies :: HasCallStack => HscEnv -> Set UnitId
hsc_currentHomeUnitDependencies e = hsc_homeUnitDependencies e (hsc_currentUnit e)

hsc_homeUnitDependencies :: HasCallStack => HscEnv -> UnitId -> Set UnitId
hsc_homeUnitDependencies hsc_env unitId =
  Map.keysSet . unitEnv_graph
    $ unitEnv_restrictHomeUnitDependencies (hsc_internalUnitEnv hsc_env) unitId

hsc_homeUnitDependencies_unitEnv :: HasCallStack => HscEnv -> UnitId -> [InternalUnitEnv]
hsc_homeUnitDependencies_unitEnv hsc_env unitId =
  Map.elems . unitEnv_graph
    $ unitEnv_restrictHomeUnitDependencies (hsc_internalUnitEnv  hsc_env) unitId

unitEnv_homeUnitDependencies :: HasCallStack => UnitEnv -> UnitId -> [UnitId]
unitEnv_homeUnitDependencies unitEnv unitId = filter (`unitEnv_member` unitEnv) deps
  where
    dflags = internalUnitEnv_dflags $ unitEnv_lookup unitId unitEnv
    deps = map toUnitId $ explicitUnits (unitState dflags)

unitEnv_restrictHomeUnitDependencies :: HasCallStack => UnitEnv -> UnitId -> UnitEnv
unitEnv_restrictHomeUnitDependencies unitEnv unitId = unitEnv_setGraph
  (Map.restrictKeys
      (unitEnv_graph unitEnv)
      deps
  )
  unitEnv
  where
    dflags = internalUnitEnv_dflags $ unitEnv_lookup unitId unitEnv    --
    -- Always remove the current home unit from the list of home unit dependencies.
    -- We do this, since if "-this-unit-id" is set to "base" (or any other wired-in unit)
    -- but we are not compiling the wired-in unit, the unit id key in 'UnitEnvGraph' will be
    -- "base-<wired-in version>" and it will also have a dependency on "base-<wired-in version>".
    -- Those units are actually not the same unit, but it will look like it.
    -- So, "base-<wired-in version>" is both home unit id and a dependency for the current unit id.
    -- Therefore, it will be in the Set of Unit Ids and violate our post-condition
    -- that the function result does not include the given Unit Id's key.
    deps = Set.delete unitId $  Set.fromList $ map toUnitId $ explicitUnits (unitState dflags)

-- -----------------------------------------------------------------------------

set_hsc_currentInternalUnitEnv :: HscEnv -> InternalUnitEnv -> HscEnv
set_hsc_currentInternalUnitEnv hsc_env unitEnv = hsc_env
  { hsc_internalUnitEnv = unitEnv_adjust (const unitEnv) (hsc_currentUnit hsc_env) (hsc_internalUnitEnv hsc_env)
  }

hsc_currentUnit :: HscEnv -> UnitId
hsc_currentUnit = unitEnv_currentUnit . hsc_internalUnitEnv

-- -- | Test if the module comes from the home unit
-- isAnyHomeModule :: HscEnv -> Module -> Bool
-- isAnyHomeModule hsc_env m = isJust $ hsc_findInternalUnitEnv_maybe (toUnitId $ moduleUnit m) hsc_env


{-

Note [Target code interpreter]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Template Haskell and GHCi use an interpreter to execute code that is built for
the compiler target platform (= code host platform) on the compiler host
platform (= code build platform).

The internal interpreter can be used when both platforms are the same and when
the built code is compatible with the compiler itself (same way, etc.). This
interpreter is not always available: for instance stage1 compiler doesn't have
it because there might be an ABI mismatch between the code objects (built by
stage1 compiler) and the stage1 compiler itself (built by stage0 compiler).

In most cases, an external interpreter can be used instead: it runs in a
separate process and it communicates with the compiler via a two-way message
passing channel. The process is lazily spawned to avoid overhead when it is not
used.

The target code interpreter to use can be selected per session via the
`hsc_interp` field of `HscEnv`. There may be no interpreter available at all, in
which case Template Haskell and GHCi will fail to run. The interpreter to use is
configured via command-line flags (in `GHC.setSessionDynFlags`).


-}

-- Note [hsc_type_env_var hack]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- hsc_type_env_var is used to initialize tcg_type_env_var, and
-- eventually it is the mutable variable that is queried from
-- if_rec_types to get a TypeEnv.  So, clearly, it's something
-- related to knot-tying (see Note [Tying the knot]).
-- hsc_type_env_var is used in two places: initTcRn (where
-- it initializes tcg_type_env_var) and initIfaceCheck
-- (where it initializes if_rec_types).
--
-- But why do we need a way to feed a mutable variable in?  Why
-- can't we just initialize tcg_type_env_var when we start
-- typechecking?  The problem is we need to knot-tie the
-- EPS, and we may start adding things to the EPS before type
-- checking starts.
--
-- Here is a concrete example. Suppose we are running
-- "ghc -c A.hs", and we have this file system state:
--
--  A.hs-boot   A.hi-boot **up to date**
--  B.hs        B.hi      **up to date**
--  A.hs        A.hi      **stale**
--
-- The first thing we do is run checkOldIface on A.hi.
-- checkOldIface will call loadInterface on B.hi so it can
-- get its hands on the fingerprints, to find out if A.hi
-- needs recompilation.  But loadInterface also populates
-- the EPS!  And so if compilation turns out to be necessary,
-- as it is in this case, the thunks we put into the EPS for
-- B.hi need to have the correct if_rec_types mutable variable
-- to query.
--
-- If the mutable variable is only allocated WHEN we start
-- typechecking, then that's too late: we can't get the
-- information to the thunks.  So we need to pre-commit
-- to a type variable in 'hscIncrementalCompile' BEFORE we
-- check the old interface.
--
-- This is all a massive hack because arguably checkOldIface
-- should not populate the EPS. But that's a refactor for
-- another day.

-- | Retrieve the ExternalPackageState cache.
hscEPS :: HscEnv -> IO ExternalPackageState
hscEPS hsc_env = readIORef (hsc_EPS hsc_env)

hptCompleteSigs :: HscEnv -> [CompleteMatch]
hptCompleteSigs = hptAllThings  (md_complete_matches . hm_details)

-- | Find all the instance declarations (of classes and families) from
-- the Home Package Table filtered by the provided predicate function.
-- Used in @tcRnImports@, to select the instances that are in the
-- transitive closure of imports from the currently compiled module.
hptInstances :: HscEnv -> (ModuleName -> Bool) -> ([ClsInst], [FamInst])
hptInstances hsc_env want_this_module
  = let (insts, famInsts) = unzip $ flip hptAllThings hsc_env $ \mod_info -> do
                guard (want_this_module (moduleName (mi_module (hm_iface mod_info))))
                let details = hm_details mod_info
                return (md_insts details, md_fam_insts details)
    in (concat insts, concat famInsts)

-- | Get rules from modules "below" this one (in the dependency sense)
hptRules :: HscEnv -> [ModuleNameWithIsBoot] -> [CoreRule]
hptRules = hptSomeThingsBelowUs (md_rules . hm_details) False


-- | Get annotations from modules "below" this one (in the dependency sense)
hptAnns :: HscEnv -> Maybe [ModuleNameWithIsBoot] -> [Annotation]
hptAnns hsc_env (Just deps) = hptSomeThingsBelowUs (md_anns . hm_details) False hsc_env deps
hptAnns hsc_env Nothing = hptAllThings (md_anns . hm_details) hsc_env

hptAllThings :: (HomeModInfo -> [a]) -> HscEnv -> [a]
hptAllThings extract hsc_env = concatMap extract (eltsHpt (hsc_HPT hsc_env))

-- | Get things from modules "below" this one (in the dependency sense)
-- C.f Inst.hptInstances
hptSomeThingsBelowUs :: (HomeModInfo -> [a]) -> Bool -> HscEnv -> [ModuleNameWithIsBoot] -> [a]
hptSomeThingsBelowUs extract include_hi_boot hsc_env deps
  | isOneShot (ghcMode (hsc_dflags hsc_env)) = []

  | otherwise
  = let hpt = hsc_HPT hsc_env
    in
    [ thing
    |   -- Find each non-hi-boot module below me
      GWIB { gwib_mod = mod, gwib_isBoot = is_boot } <- deps
    , include_hi_boot || (is_boot == NotBoot)

        -- unsavoury: when compiling the base package with --make, we
        -- sometimes try to look up RULES etc for GHC.Prim. GHC.Prim won't
        -- be in the HPT, because we never compile it; it's in the EPT
        -- instead. ToDo: clean up, and remove this slightly bogus filter:
    , mod /= moduleName gHC_PRIM

        -- Look it up in the HPT
    , let things = case lookupHpt hpt mod of
                    Just info -> extract info
                    Nothing -> pprTrace "WARNING in hptSomeThingsBelowUs" msg []
          msg = vcat [text "missing module" <+> ppr mod,
                      text "Probable cause: out-of-date interface files"]
                        -- This really shouldn't happen, but see #962

        -- And get its dfuns
    , thing <- things ]


-- | Deal with gathering annotations in from all possible places
--   and combining them into a single 'AnnEnv'
prepareAnnotations :: HscEnv -> Maybe ModGuts -> IO AnnEnv
prepareAnnotations hsc_env mb_guts = do
    eps <- hscEPS hsc_env
    let -- Extract annotations from the module being compiled if supplied one
        mb_this_module_anns = fmap (mkAnnEnv . mg_anns) mb_guts
        -- Extract dependencies of the module if we are supplied one,
        -- otherwise load annotations from all home package table
        -- entries regardless of dependency ordering.
        home_pkg_anns  = (mkAnnEnv . hptAnns hsc_env) $ fmap (dep_mods . mg_deps) mb_guts
        other_pkg_anns = eps_ann_env eps
        ann_env        = foldl1' plusAnnEnv $ catMaybes [mb_this_module_anns,
                                                         Just home_pkg_anns,
                                                         Just other_pkg_anns]
    return ann_env

-- | Find the 'TyThing' for the given 'Name' by using all the resources
-- at our disposal: the compiled modules in the 'HomePackageTable' and the
-- compiled modules in other packages that live in 'PackageTypeEnv'. Note
-- that this does NOT look up the 'TyThing' in the module being compiled: you
-- have to do that yourself, if desired
lookupType :: HscEnv -> Name -> IO (Maybe TyThing)
lookupType hsc_env name = do
   eps <- liftIO $ readIORef (hsc_EPS hsc_env)
   let pte = eps_PTE eps
       hpt = hsc_HPT hsc_env

       mod = ASSERT2( isExternalName name, ppr name )
             if isHoleName name
               then mkHomeModule (hsc_home_unit hsc_env) (moduleName (nameModule name))
               else nameModule name

       !ty = if isOneShot (ghcMode (hsc_dflags hsc_env))
               -- in one-shot, we don't use the HPT
               then lookupNameEnv pte name
               else case lookupHptByModule hpt mod of
                Just hm -> lookupNameEnv (md_types (hm_details hm)) name
                Nothing -> lookupNameEnv pte name
   pure ty

-- | Find the 'ModIface' for a 'Module', searching in both the loaded home
-- and external package module information
lookupIfaceByModule
        :: HomePackageTable
        -> PackageIfaceTable
        -> Module
        -> Maybe ModIface
lookupIfaceByModule hpt pit mod
  = case lookupHptByModule hpt mod of
       Just hm -> Just (hm_iface hm)
       Nothing -> lookupModuleEnv pit mod
   -- If the module does come from the home package, why do we look in the PIT as well?
   -- (a) In OneShot mode, even home-package modules accumulate in the PIT
   -- (b) Even in Batch (--make) mode, there is *one* case where a home-package
   --     module is in the PIT, namely GHC.Prim when compiling the base package.
   -- We could eliminate (b) if we wanted, by making GHC.Prim belong to a package
   -- of its own, but it doesn't seem worth the bother.

