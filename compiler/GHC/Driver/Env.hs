{-# LANGUAGE LambdaCase #-}

module GHC.Driver.Env
   ( Hsc(..)
   , HscEnv (..)
   , hscUpdateFlags
   , hscSetFlags
   , hsc_home_unit
   , hsc_home_unit_maybe
   , hsc_units
   , hsc_HPT
   , hscUpdateHPT
   , hscUpdateLoggerFlags
   , runHsc
   , runHsc'
   , mkInteractiveHscEnv
   , runInteractiveHsc
   , hscEPS
   , hscInterp
   , hptCompleteSigs
   , hptAllInstances
   , hptInstancesBelow
   , hptAnns
   , hptAllThings
   , hptSomeThingsBelowUs
   , hptRules
   , prepareAnnotations
   , lookupType
   , lookupIfaceByModule
   , mainModIs
   )
where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Driver.Errors ( printOrThrowDiagnostics )
import GHC.Driver.Errors.Types ( GhcMessage )
import GHC.Driver.Config.Logger (initLogFlags)
import GHC.Driver.Config.Diagnostic (initDiagOpts)
import GHC.Driver.Env.Types ( Hsc(..), HscEnv(..) )

import GHC.Runtime.Context
import GHC.Runtime.Interpreter.Types (Interp)

import GHC.Unit
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.Deps
import GHC.Unit.Home.ModInfo
import GHC.Unit.Env
import GHC.Unit.External

import GHC.Core         ( CoreRule )
import GHC.Core.FamInstEnv
import GHC.Core.InstEnv ( ClsInst )

import GHC.Types.Annotations ( Annotation, AnnEnv, mkAnnEnv, plusAnnEnv )
import GHC.Types.CompleteMatch
import GHC.Types.Error ( emptyMessages, Messages )
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.TyThing

import GHC.Builtin.Names ( gHC_PRIM )

import GHC.Data.Maybe

import GHC.Utils.Exception as Ex
import GHC.Utils.Outputable
import GHC.Utils.Monad
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Logger
import GHC.Utils.Trace

import Data.IORef
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Unit.Module.Graph
import Data.List (sort)
import qualified Data.Map as Map

runHsc :: HscEnv -> Hsc a -> IO a
runHsc hsc_env (Hsc hsc) = do
    (a, w) <- hsc hsc_env emptyMessages
    let dflags = hsc_dflags hsc_env
    let !diag_opts = initDiagOpts dflags
    printOrThrowDiagnostics (hsc_logger hsc_env) diag_opts w
    return a

runHsc' :: HscEnv -> Hsc a -> IO (a, Messages GhcMessage)
runHsc' hsc_env (Hsc hsc) = hsc hsc_env emptyMessages

-- | Switches in the DynFlags and Plugins from the InteractiveContext
mkInteractiveHscEnv :: HscEnv -> HscEnv
mkInteractiveHscEnv hsc_env =
    let ic = hsc_IC hsc_env
    in hscSetFlags (ic_dflags ic) $
       hsc_env { hsc_plugins = ic_plugins ic }

-- | A variant of runHsc that switches in the DynFlags and Plugins from the
-- InteractiveContext before running the Hsc computation.
runInteractiveHsc :: HscEnv -> Hsc a -> IO a
runInteractiveHsc hsc_env = runHsc (mkInteractiveHscEnv hsc_env)

hsc_home_unit :: HscEnv -> HomeUnit
hsc_home_unit = unsafeGetHomeUnit . hsc_unit_env

hsc_home_unit_maybe :: HscEnv -> Maybe HomeUnit
hsc_home_unit_maybe = ue_home_unit . hsc_unit_env

hsc_units :: HscEnv -> UnitState
hsc_units = ue_units . hsc_unit_env

hsc_HPT :: HscEnv -> HomePackageTable
hsc_HPT = ue_hpt . hsc_unit_env

hscUpdateHPT :: (HomePackageTable -> HomePackageTable) -> HscEnv -> HscEnv
hscUpdateHPT f hsc_env = hsc_env { hsc_unit_env = updateHpt f (hsc_unit_env hsc_env) }

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
hscEPS hsc_env = readIORef (euc_eps (ue_eps (hsc_unit_env hsc_env)))

hptCompleteSigs :: HscEnv -> [CompleteMatch]
hptCompleteSigs = hptAllThings  (md_complete_matches . hm_details)

-- | Find all the instance declarations (of classes and families) from
-- the Home Package Table filtered by the provided predicate function.
-- Used in @tcRnImports@, to select the instances that are in the
-- transitive closure of imports from the currently compiled module.
hptAllInstances :: HscEnv -> ([ClsInst], [FamInst])
hptAllInstances hsc_env
  = let (insts, famInsts) = unzip $ flip hptAllThings hsc_env $ \mod_info -> do
                let details = hm_details mod_info
                return (md_insts details, md_fam_insts details)
    in (concat insts, concat famInsts)

-- | Find instances visible from the given set of imports
hptInstancesBelow :: HscEnv -> ModuleName -> Set ModuleNameWithIsBoot -> ([ClsInst], [FamInst])
hptInstancesBelow hsc_env mn mns =
  let (insts, famInsts) =
        unzip $ hptSomeThingsBelowUs (\mod_info ->
                                     let details = hm_details mod_info
                                     -- Don't include instances for the current module
                                     in if moduleName (mi_module (hm_iface mod_info)) == mn
                                          then []
                                          else [(md_insts details, md_fam_insts details)])
                             True -- Include -hi-boot
                             hsc_env
                             mns
  in (concat insts, concat famInsts)

-- | Get rules from modules "below" this one (in the dependency sense)
hptRules :: HscEnv -> Set ModuleNameWithIsBoot -> [CoreRule]
hptRules = hptSomeThingsBelowUs (md_rules . hm_details) False


-- | Get annotations from modules "below" this one (in the dependency sense)
hptAnns :: HscEnv -> Maybe (Set ModuleNameWithIsBoot) -> [Annotation]
hptAnns hsc_env (Just deps) = hptSomeThingsBelowUs (md_anns . hm_details) False hsc_env deps
hptAnns hsc_env Nothing = hptAllThings (md_anns . hm_details) hsc_env

hptAllThings :: (HomeModInfo -> [a]) -> HscEnv -> [a]
hptAllThings extract hsc_env = concatMap extract (eltsHpt (hsc_HPT hsc_env))

-- | This function returns all the modules belonging to the home-unit that can
-- be reached by following the given dependencies. Additionally, if both the
-- boot module and the non-boot module can be reached, it only returns the
-- non-boot one.
hptModulesBelow :: HscEnv -> Set ModuleNameWithIsBoot -> Set ModuleNameWithIsBoot
hptModulesBelow hsc_env mn = filtered_mods $ [ mnwib |  NodeKey_Module mnwib <- modules_below]
  where
    td_map = mgTransDeps (hsc_mod_graph hsc_env)

    modules_below = Set.toList (Set.unions (mapMaybe (\mn -> Map.lookup (NodeKey_Module mn) td_map) (Set.toList mn))
                               `Set.union` (Set.map NodeKey_Module mn))

    filtered_mods = Set.fromDistinctAscList . filter_mods . sort

    -- IsBoot and NotBoot modules are necessarily consecutive in the sorted list
    -- (cf Ord instance of GenWithIsBoot). Hence we only have to perform a
    -- linear sweep with a window of size 2 to remove boot modules for which we
    -- have the corresponding non-boot.
    filter_mods = \case
      (r1@(GWIB m1 b1) : r2@(GWIB m2 _) : rs)
        | m1 == m2  -> let !r' = case b1 of
                                  NotBoot -> r1
                                  IsBoot  -> r2
                       in r' : filter_mods rs
        | otherwise -> r1 : filter_mods (r2:rs)
      rs -> rs



-- | Get things from modules "below" this one (in the dependency sense)
-- C.f Inst.hptInstances
hptSomeThingsBelowUs :: (HomeModInfo -> [a]) -> Bool -> HscEnv -> Set ModuleNameWithIsBoot -> [a]
hptSomeThingsBelowUs extract include_hi_boot hsc_env deps
  | isOneShot (ghcMode (hsc_dflags hsc_env)) = []

  | otherwise
  = let hpt = hsc_HPT hsc_env
    in
    [ thing
    |   -- Find each non-hi-boot module below me
      GWIB { gwib_mod = mod, gwib_isBoot = is_boot } <- Set.toList (hptModulesBelow hsc_env deps)
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
        home_pkg_anns  = (mkAnnEnv . hptAnns hsc_env) $ fmap (dep_direct_mods . mg_deps) mb_guts
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
   eps <- liftIO $ hscEPS hsc_env
   let pte = eps_PTE eps
       hpt = hsc_HPT hsc_env

       mod = assertPpr (isExternalName name) (ppr name) $
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

mainModIs :: HscEnv -> Module
mainModIs hsc_env = mkHomeModule (hsc_home_unit hsc_env) (mainModuleNameIs (hsc_dflags hsc_env))

-- | Retrieve the target code interpreter
--
-- Fails if no target code interpreter is available
hscInterp :: HscEnv -> Interp
hscInterp hsc_env = case hsc_interp hsc_env of
   Nothing -> throw (InstallationError "Couldn't find a target code interpreter. Try with -fexternal-interpreter")
   Just i  -> i

-- | Update the LogFlags of the Log in hsc_logger from the DynFlags in
-- hsc_dflags. You need to call this when DynFlags are modified.
hscUpdateLoggerFlags :: HscEnv -> HscEnv
hscUpdateLoggerFlags h = h
  { hsc_logger = setLogFlags (hsc_logger h) (initLogFlags (hsc_dflags h)) }

-- | Update Flags
hscUpdateFlags :: (DynFlags -> DynFlags) -> HscEnv -> HscEnv
hscUpdateFlags f h = hscSetFlags (f (hsc_dflags h)) h

-- | Set Flags
hscSetFlags :: DynFlags -> HscEnv -> HscEnv
hscSetFlags dflags h =
  -- update LogFlags from the new DynFlags
  hscUpdateLoggerFlags
  $ h { hsc_dflags = dflags }
