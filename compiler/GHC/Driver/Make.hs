{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2011
--
-- This module implements multi-module compilation, and is used
-- by --make and GHCi.
--
-- -----------------------------------------------------------------------------
module GHC.Driver.Make (
        depanal, depanalE, depanalPartial, checkHomeUnitsClosed,
        load, loadWithCache, load', AnyGhcDiagnostic, LoadHowMuch(..), ModIfaceCache(..), noIfaceCache, newIfaceCache,
        instantiationNodes,

        downsweep,

        topSortModuleGraph,

        ms_home_srcimps, ms_home_imps,

        summariseModule,
        SummariseResult(..),
        summariseFile,
        hscSourceToIsBoot,
        findExtraSigImports,
        implicitRequirementsShallow,

        noModError, cyclicModuleErr,
        SummaryNode,
        IsBootInterface(..), mkNodeKey,

        ModNodeKey, ModNodeKeyWithUid(..),
        ModNodeMap(..), emptyModNodeMap, modNodeMapElems, modNodeMapLookup, modNodeMapInsert, modNodeMapSingleton, modNodeMapUnionWith
        ) where

import GHC.Prelude
import GHC.Platform

import GHC.Tc.Utils.Backpack
import GHC.Tc.Utils.Monad  ( initIfaceCheck, concatMapM )

import GHC.Runtime.Interpreter
import qualified GHC.Linker.Loader as Linker
import GHC.Linker.Types

import GHC.Platform.Ways

import GHC.Driver.Config.Finder (initFinderOpts)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Phases
import GHC.Driver.Pipeline
import GHC.Driver.Session
import GHC.Driver.DynFlags (ReexportedModule(..))
import GHC.Driver.Backend
import GHC.Driver.Monad
import GHC.Driver.Env
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Main
import GHC.Driver.MakeSem

import GHC.Parser.Header
import GHC.ByteCode.Types

import GHC.Iface.Load      ( cannotFindModule )
import GHC.IfaceToCore     ( typecheckIface )
import GHC.Iface.Recomp    ( RecompileRequired(..), CompileReason(..) )

import GHC.Data.Bag        ( listToBag )
import GHC.Data.Graph.Directed
import GHC.Data.FastString
import GHC.Data.Maybe      ( expectJust )
import GHC.Data.OsPath     ( unsafeEncodeUtf )
import GHC.Data.StringBuffer
import qualified GHC.LanguageExtensions as LangExt

import GHC.Utils.Exception ( throwIO, SomeAsyncException )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Error
import GHC.Utils.Logger
import GHC.Utils.Fingerprint
import GHC.Utils.TmpFs

import GHC.Types.Basic
import GHC.Types.Error
import GHC.Types.Target
import GHC.Types.SourceFile
import GHC.Types.SourceError
import GHC.Types.SrcLoc
import GHC.Types.Unique.Map
import GHC.Types.PkgQual

import GHC.Unit
import GHC.Unit.Env
import GHC.Unit.Finder
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Graph
import GHC.Unit.Home.ModInfo
import GHC.Unit.Module.ModDetails

import Data.Either ( rights, partitionEithers, lefts )
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Concurrent ( newQSem, waitQSem, signalQSem, ThreadId, killThread, forkIOWithUnmask )
import qualified GHC.Conc as CC
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans.Except ( ExceptT(..), runExceptT, throwE )
import qualified Control.Monad.Catch as MC
import Data.IORef
import Data.Maybe
import Data.Time
import Data.List (sortOn, unfoldr)
import Data.Bifunctor (first)
import System.Directory
import System.FilePath
import System.IO        ( fixIO )

import GHC.Conc ( getNumProcessors, getNumCapabilities, setNumCapabilities )
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import GHC.Driver.Pipeline.LogQueue
import qualified Data.Map.Strict as M
import GHC.Types.TypeEnv
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import GHC.Driver.Env.KnotVars
import Control.Concurrent.STM
import Control.Monad.Trans.Maybe
import GHC.Runtime.Loader
import GHC.Rename.Names
import GHC.Utils.Constants
import GHC.Types.Unique.DFM (udfmRestrictKeysSet)
import GHC.Types.Unique
import GHC.Iface.Errors.Types

import qualified GHC.Data.Word64Set as W

-- -----------------------------------------------------------------------------
-- Loading the program

-- | Perform a dependency analysis starting from the current targets
-- and update the session with the new module graph.
--
-- Dependency analysis entails parsing the @import@ directives and may
-- therefore require running certain preprocessors.
--
-- Note that each 'ModSummary' in the module graph caches its 'DynFlags'.
-- These 'DynFlags' are determined by the /current/ session 'DynFlags' and the
-- @OPTIONS@ and @LANGUAGE@ pragmas of the parsed module.  Thus if you want
-- changes to the 'DynFlags' to take effect you need to call this function
-- again.
-- In case of errors, just throw them.
--
depanal :: GhcMonad m =>
           [ModuleName]  -- ^ excluded modules
        -> Bool          -- ^ allow duplicate roots
        -> m ModuleGraph
depanal excluded_mods allow_dup_roots = do
    (errs, mod_graph) <- depanalE mkUnknownDiagnostic Nothing excluded_mods allow_dup_roots
    if isEmptyMessages errs
      then pure mod_graph
      else throwErrors (fmap GhcDriverMessage errs)

-- | Perform dependency analysis like in 'depanal'.
-- In case of errors, the errors and an empty module graph are returned.
depanalE :: GhcMonad m =>     -- New for #17459
               (GhcMessage -> AnyGhcDiagnostic)
            -> Maybe Messager
            -> [ModuleName]      -- ^ excluded modules
            -> Bool           -- ^ allow duplicate roots
            -> m (DriverMessages, ModuleGraph)
depanalE diag_wrapper msg excluded_mods allow_dup_roots = do
    hsc_env <- getSession
    (errs, mod_graph) <- depanalPartial diag_wrapper msg excluded_mods allow_dup_roots
    if isEmptyMessages errs
      then do
        hsc_env <- getSession
        let one_unit_messages get_mod_errs k hue = do
              errs <- get_mod_errs
              unknown_module_err <- warnUnknownModules (hscSetActiveUnitId k hsc_env) (homeUnitEnv_dflags hue) mod_graph

              let unused_home_mod_err = warnMissingHomeModules (homeUnitEnv_dflags hue) (hsc_targets hsc_env) mod_graph
                  unused_pkg_err = warnUnusedPackages (homeUnitEnv_units hue) (homeUnitEnv_dflags hue) mod_graph


              return $ errs `unionMessages` unused_home_mod_err
                          `unionMessages` unused_pkg_err
                          `unionMessages` unknown_module_err

        all_errs <- liftIO $ unitEnv_foldWithKey one_unit_messages (return emptyMessages) (hsc_HUG hsc_env)
        logDiagnostics (GhcDriverMessage <$> all_errs)
        setSession hsc_env { hsc_mod_graph = mod_graph }
        pure (emptyMessages, mod_graph)
      else do
        -- We don't have a complete module dependency graph,
        -- The graph may be disconnected and is unusable.
        setSession hsc_env { hsc_mod_graph = emptyMG }
        pure (errs, emptyMG)


-- | Perform dependency analysis like 'depanal' but return a partial module
-- graph even in the face of problems with some modules.
--
-- Modules which have parse errors in the module header, failing
-- preprocessors or other issues preventing them from being summarised will
-- simply be absent from the returned module graph.
--
-- Unlike 'depanal' this function will not update 'hsc_mod_graph' with the
-- new module graph.
depanalPartial
    :: GhcMonad m
    => (GhcMessage -> AnyGhcDiagnostic)
    -> Maybe Messager
    -> [ModuleName]  -- ^ excluded modules
    -> Bool          -- ^ allow duplicate roots
    -> m (DriverMessages, ModuleGraph)
    -- ^ possibly empty 'Bag' of errors and a module graph.
depanalPartial diag_wrapper msg excluded_mods allow_dup_roots = do
  hsc_env <- getSession
  let
         targets = hsc_targets hsc_env
         old_graph = hsc_mod_graph hsc_env
         logger  = hsc_logger hsc_env

  withTiming logger (text "Chasing dependencies") (const ()) $ do
    liftIO $ debugTraceMsg logger 2 (hcat [
              text "Chasing modules from: ",
              hcat (punctuate comma (map pprTarget targets))])

    -- Home package modules may have been moved or deleted, and new
    -- source files may have appeared in the home package that shadow
    -- external package modules, so we have to discard the existing
    -- cached finder data.
    liftIO $ flushFinderCaches (hsc_FC hsc_env) (hsc_unit_env hsc_env)

    (errs, graph_nodes) <- liftIO $ downsweep
      hsc_env diag_wrapper msg (mgModSummaries old_graph)
      excluded_mods allow_dup_roots
    let
      mod_graph = mkModuleGraph graph_nodes
    return (unionManyMessages errs, mod_graph)

-- | Collect the instantiations of dependencies to create 'InstantiationNode' work graph nodes.
-- These are used to represent the type checking that is done after
-- all the free holes (sigs in current package) relevant to that instantiation
-- are compiled. This is necessary to catch some instantiation errors.
--
-- In the future, perhaps more of the work of instantiation could be moved here,
-- instead of shoved in with the module compilation nodes. That could simplify
-- backpack, and maybe hs-boot too.
instantiationNodes :: UnitId -> UnitState -> [ModuleGraphNode]
instantiationNodes uid unit_state = InstantiationNode uid <$> iuids_to_check
  where
    iuids_to_check :: [InstantiatedUnit]
    iuids_to_check =
      nubSort $ concatMap (goUnitId . fst) (explicitUnits unit_state)
     where
      goUnitId uid =
        [ recur
        | VirtUnit indef <- [uid]
        , inst <- instUnitInsts indef
        , recur <- (indef :) $ goUnitId $ moduleUnit $ snd inst
        ]

-- The linking plan for each module. If we need to do linking for a home unit
-- then this function returns a graph node which depends on all the modules in the home unit.

-- At the moment nothing can depend on these LinkNodes.
linkNodes :: [ModuleGraphNode] -> UnitId -> HomeUnitEnv -> Maybe (Either (Messages DriverMessage) ModuleGraphNode)
linkNodes summaries uid hue =
  let dflags = homeUnitEnv_dflags hue
      ofile = outputFile_ dflags

      unit_nodes :: [NodeKey]
      unit_nodes = map mkNodeKey (filter ((== uid) . moduleGraphNodeUnitId) summaries)
  -- Issue a warning for the confusing case where the user
  -- said '-o foo' but we're not going to do any linking.
  -- We attempt linking if either (a) one of the modules is
  -- called Main, or (b) the user said -no-hs-main, indicating
  -- that main() is going to come from somewhere else.
  --
      no_hs_main = gopt Opt_NoHsMain dflags

      main_sum = any (== NodeKey_Module (ModNodeKeyWithUid (GWIB (mainModuleNameIs dflags) NotBoot) uid)) unit_nodes

      do_linking =  main_sum || no_hs_main || ghcLink dflags == LinkDynLib || ghcLink dflags == LinkStaticLib

  in if | ghcLink dflags == LinkBinary && isJust ofile && not do_linking ->
            Just (Left $ singleMessage $ mkPlainErrorMsgEnvelope noSrcSpan (DriverRedirectedNoMain $ mainModuleNameIs dflags))
        -- This should be an error, not a warning (#10895).
        | ghcLink dflags /= NoLink, do_linking -> Just (Right (LinkNode unit_nodes uid))
        | otherwise  -> Nothing

-- Note [Missing home modules]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Sometimes we don't want GHC to process modules that weren't specified as
-- explicit targets. For example, cabal may want to enable this warning
-- when building a library, so that GHC warns the user about modules listed
-- neither in `exposed-modules` nor in `other-modules`.
--
-- Here "home module" means a module that doesn't come from another package.
--
-- For example, if GHC is invoked with modules "A" and "B" as targets,
-- but "A" imports some other module "C", then GHC will issue a warning
-- about module "C" not being listed in the command line.
--
-- The warning in enabled by `-Wmissing-home-modules`. See #13129
warnMissingHomeModules ::  DynFlags -> [Target] -> ModuleGraph -> DriverMessages
warnMissingHomeModules dflags targets mod_graph =
    if null missing
      then emptyMessages
      else warn
  where
    diag_opts = initDiagOpts dflags

    -- We need to be careful to handle the case where (possibly
    -- path-qualified) filenames (aka 'TargetFile') rather than module
    -- names are being passed on the GHC command-line.
    --
    -- For instance, `ghc --make src-exe/Main.hs` and
    -- `ghc --make -isrc-exe Main` are supposed to be equivalent.
    -- Note also that we can't always infer the associated module name
    -- directly from the filename argument.  See #13727.
    is_known_module mod =
      is_module_target mod
      ||
      maybe False is_file_target (ml_hs_file (ms_location mod))

    is_module_target mod = (moduleName (ms_mod mod), ms_unitid mod) `Set.member` mod_targets

    is_file_target file = Set.member (withoutExt file) file_targets

    file_targets = Set.fromList (mapMaybe file_target targets)

    file_target Target {targetId} =
      case targetId of
        TargetModule _ -> Nothing
        TargetFile file _ ->
          Just (withoutExt (augmentByWorkingDirectory dflags file))

    mod_targets = Set.fromList (mod_target <$> targets)

    mod_target Target {targetUnitId, targetId} =
      case targetId of
        TargetModule name -> (name, targetUnitId)
        TargetFile file _ -> (mkModuleName (withoutExt file), targetUnitId)

    withoutExt = fst . splitExtension

    missing = map (moduleName . ms_mod) $
      filter (not . is_known_module) $
        (filter (\ms -> ms_unitid ms == homeUnitId_ dflags)
                (mgModSummaries mod_graph))

    warn = singleMessage $ mkPlainMsgEnvelope diag_opts noSrcSpan
                         $ DriverMissingHomeModules (homeUnitId_ dflags) missing (checkBuildingCabalPackage dflags)

-- Check that any modules we want to reexport or hide are actually in the package.
warnUnknownModules :: HscEnv -> DynFlags -> ModuleGraph -> IO DriverMessages
warnUnknownModules hsc_env dflags mod_graph = do
  reexported_warns <- filterM check_reexport reexported_mods
  return $ final_msgs hidden_warns reexported_warns
  where
    diag_opts = initDiagOpts dflags

    unit_mods = Set.fromList (map ms_mod_name
                  (filter (\ms -> ms_unitid ms == homeUnitId_ dflags)
                       (mgModSummaries mod_graph)))

    reexported_mods = reexportedModules dflags
    hidden_mods     = hiddenModules dflags

    hidden_warns = hidden_mods `Set.difference` unit_mods

    lookupModule mn = findImportedModule hsc_env mn NoPkgQual

    check_reexport mn = do
      fr <- lookupModule (reexportFrom mn)
      case fr of
        Found _ m -> return (moduleUnitId m == homeUnitId_ dflags)
        _ -> return True


    warn diagnostic = singleMessage $ mkPlainMsgEnvelope diag_opts noSrcSpan
                         $ diagnostic

    final_msgs hidden_warns reexported_warns
          =
        unionManyMessages $
          [warn (DriverUnknownHiddenModules (homeUnitId_ dflags) (Set.toList hidden_warns)) | not (Set.null hidden_warns)]
          ++ [warn (DriverUnknownReexportedModules (homeUnitId_ dflags) reexported_warns) | not (null reexported_warns)]

-- | Describes which modules of the module graph need to be loaded.
data LoadHowMuch
   = LoadAllTargets
     -- ^ Load all targets and its dependencies.
   | LoadUpTo HomeUnitModule
     -- ^ Load only the given module and its dependencies.
   | LoadDependenciesOf HomeUnitModule
     -- ^ Load only the dependencies of the given module, but not the module
     -- itself.

{-
Note [Caching HomeModInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~

API clients who call `load` like to cache the HomeModInfo in memory between
calls to this function. In the old days, this cache was a simple MVar which stored
a HomePackageTable. This was insufficient, as the interface files for boot modules
were not recorded in the cache. In the less old days, the cache was returned at the
end of load, and supplied at the start of load, however, this was not sufficient
because it didn't account for the possibility of exceptions such as SIGINT (#20780).

So now, in the current day, we have this ModIfaceCache abstraction which
can incrementally be updated during the process of upsweep. This allows us
to store interface files for boot modules in an exception-safe way.

When the final version of an interface file is completed then it is placed into
the cache. The contents of the cache is retrieved, and the cache cleared, by iface_clearCache.

Note that because we only store the ModIface and Linkable in the ModIfaceCache,
hydration and rehydration is totally irrelevant, and we just store the CachedIface as
soon as it is completed.

-}


-- Abstract interface to a cache of HomeModInfo
-- See Note [Caching HomeModInfo]
data ModIfaceCache = ModIfaceCache { iface_clearCache :: IO [CachedIface]
                                   , iface_addToCache :: CachedIface -> IO () }

addHmiToCache :: ModIfaceCache -> HomeModInfo -> IO ()
addHmiToCache c (HomeModInfo i _ l) = iface_addToCache c (CachedIface i l)

data CachedIface = CachedIface { cached_modiface :: !ModIface
                               , cached_linkable :: !HomeModLinkable }

instance Outputable CachedIface where
  ppr (CachedIface mi ln) = hsep [text "CachedIface", ppr (miKey mi), ppr ln]

noIfaceCache :: Maybe ModIfaceCache
noIfaceCache = Nothing

newIfaceCache :: IO ModIfaceCache
newIfaceCache = do
  ioref <- newIORef []
  return $
    ModIfaceCache
      { iface_clearCache = atomicModifyIORef' ioref (\c -> ([], c))
      , iface_addToCache = \hmi -> atomicModifyIORef' ioref (\c -> (hmi:c, ()))
      }




-- | Try to load the program.  See 'LoadHowMuch' for the different modes.
--
-- This function implements the core of GHC's @--make@ mode.  It preprocesses,
-- compiles and loads the specified modules, avoiding re-compilation wherever
-- possible.  Depending on the backend (see 'DynFlags.backend' field) compiling
-- and loading may result in files being created on disk.
--
-- Calls the 'defaultWarnErrLogger' after each compiling each module, whether
-- successful or not.
--
-- If errors are encountered during dependency analysis, the module `depanalE`
-- returns together with the errors an empty ModuleGraph.
-- After processing this empty ModuleGraph, the errors of depanalE are thrown.
-- All other errors are reported using the 'defaultWarnErrLogger'.

load :: GhcMonad f => LoadHowMuch -> f SuccessFlag
load how_much = loadWithCache noIfaceCache mkUnknownDiagnostic how_much

mkBatchMsg :: HscEnv -> Messager
mkBatchMsg hsc_env =
  if length (hsc_all_home_unit_ids hsc_env) > 1
    -- This also displays what unit each module is from.
    then batchMultiMsg
    else batchMsg

type AnyGhcDiagnostic = UnknownDiagnostic (DiagnosticOpts GhcMessage)

loadWithCache :: GhcMonad m => Maybe ModIfaceCache -- ^ Instructions about how to cache interfaces as we create them.
                            -> (GhcMessage -> AnyGhcDiagnostic) -- ^ How to wrap error messages before they are displayed to a user.
                                                                -- If you are using the GHC API you can use this to override how messages
                                                                -- created during 'loadWithCache' are displayed to the user.
                            -> LoadHowMuch -- ^ How much `loadWithCache` should load
                            -> m SuccessFlag
loadWithCache cache diag_wrapper how_much = do
    msg <- mkBatchMsg <$> getSession
    (errs, mod_graph) <- depanalE diag_wrapper (Just msg) [] False                        -- #17459
    success <- load' cache how_much diag_wrapper (Just msg) mod_graph
    if isEmptyMessages errs
      then pure success
      else throwErrors (fmap GhcDriverMessage errs)

-- Note [Unused packages]
-- ~~~~~~~~~~~~~~~~~~~~~~
-- Cabal passes `-package-id` flag for each direct dependency. But GHC
-- loads them lazily, so when compilation is done, we have a list of all
-- actually loaded packages. All the packages, specified on command line,
-- but never loaded, are probably unused dependencies.

warnUnusedPackages :: UnitState -> DynFlags -> ModuleGraph -> DriverMessages
warnUnusedPackages us dflags mod_graph =
    let diag_opts = initDiagOpts dflags

        home_mod_sum = filter (\ms -> homeUnitId_ dflags == ms_unitid ms) (mgModSummaries mod_graph)

    -- Only need non-source imports here because SOURCE imports are always HPT
        loadedPackages = concat $
          mapMaybe (\(fs, mn) -> lookupModulePackage us (unLoc mn) fs)
            $ concatMap ms_imps home_mod_sum

        any_import_ghc_prim = any ms_ghc_prim_import home_mod_sum

        used_args = Set.fromList (map unitId loadedPackages)
                      `Set.union` Set.fromList [ primUnitId |  any_import_ghc_prim ]

        resolve (u,mflag) = do
                  -- The units which we depend on via the command line explicitly
                  flag <- mflag
                  -- Which we can find the UnitInfo for (should be all of them)
                  ui <- lookupUnit us u
                  -- Which are not explicitly used
                  guard (Set.notMember (unitId ui) used_args)
                  return (unitId ui, unitPackageName ui, unitPackageVersion ui, flag)

        unusedArgs = sortOn (\(u,_,_,_) -> u) $ mapMaybe resolve (explicitUnits us)

        warn = singleMessage $ mkPlainMsgEnvelope diag_opts noSrcSpan (DriverUnusedPackages unusedArgs)

    in if null unusedArgs
        then emptyMessages
        else warn

-- | A ModuleGraphNode which also has a hs-boot file, and the list of nodes on any
-- path from module to its boot file.
data ModuleGraphNodeWithBootFile
  = ModuleGraphNodeWithBootFile
     ModuleGraphNode
       -- ^ The module itself (not the hs-boot module)
     [NodeKey]
       -- ^ The modules in between the module and its hs-boot file,
       -- not including the hs-boot file itself.


instance Outputable ModuleGraphNodeWithBootFile where
  ppr (ModuleGraphNodeWithBootFile mgn deps) = text "ModeGraphNodeWithBootFile: " <+> ppr mgn $$ ppr deps

-- | A 'BuildPlan' is the result of attempting to linearise a single strongly-connected
-- component of the module graph.
data BuildPlan
  -- | A simple, single module all alone (which *might* have an hs-boot file, if it isn't part of a cycle)
  = SingleModule ModuleGraphNode
  -- | A resolved cycle, linearised by hs-boot files
  | ResolvedCycle [Either ModuleGraphNode ModuleGraphNodeWithBootFile]
  -- | An actual cycle, which wasn't resolved by hs-boot files
  | UnresolvedCycle [ModuleGraphNode]

instance Outputable BuildPlan where
  ppr (SingleModule mgn) = text "SingleModule" <> parens (ppr mgn)
  ppr (ResolvedCycle mgn)   = text "ResolvedCycle:" <+> ppr mgn
  ppr (UnresolvedCycle mgn) = text "UnresolvedCycle:" <+> ppr mgn


-- Just used for an assertion
countMods :: BuildPlan -> Int
countMods (SingleModule _) = 1
countMods (ResolvedCycle ns) = length ns
countMods (UnresolvedCycle ns) = length ns

-- See Note [Upsweep] for a high-level description.
createBuildPlan :: ModuleGraph -> Maybe HomeUnitModule -> [BuildPlan]
createBuildPlan mod_graph maybe_top_mod =
    let -- Step 1: Compute SCCs without .hi-boot files, to find the cycles
        cycle_mod_graph = topSortModuleGraph True mod_graph maybe_top_mod

        -- Step 2: Reanalyse loops, with relevant boot modules, to solve the cycles.
        build_plan :: [BuildPlan]
        build_plan
          -- Fast path, if there are no boot modules just do a normal toposort
          | isEmptyModuleEnv boot_modules = collapseAcyclic $ topSortModuleGraph False mod_graph maybe_top_mod
          | otherwise = toBuildPlan cycle_mod_graph []

        toBuildPlan :: [SCC ModuleGraphNode] -> [ModuleGraphNode] -> [BuildPlan]
        toBuildPlan [] mgn = collapseAcyclic (topSortWithBoot mgn)
        toBuildPlan ((AcyclicSCC node):sccs) mgn = toBuildPlan sccs (node:mgn)
        -- Interesting case
        toBuildPlan ((CyclicSCC nodes):sccs) mgn =
          let acyclic = collapseAcyclic (topSortWithBoot mgn)
              -- Now perform another toposort but just with these nodes and relevant hs-boot files.
              -- The result should be acyclic, if it's not, then there's an unresolved cycle in the graph.
              mresolved_cycle = collapseSCC (topSortWithBoot nodes)
          in acyclic ++ [either UnresolvedCycle ResolvedCycle mresolved_cycle] ++ toBuildPlan sccs []

        (mg, lookup_node) = moduleGraphNodes False (mgModSummaries' mod_graph)
        trans_deps_map = allReachable mg (mkNodeKey . node_payload)
        -- Compute the intermediate modules between a file and its hs-boot file.
        -- See Step 2a in Note [Upsweep]
        boot_path mn uid =
          map (summaryNodeSummary . expectJust "toNode" . lookup_node) $ Set.toList $
          -- Don't include the boot module itself
          Set.delete (NodeKey_Module (key IsBoot))  $
          -- Keep intermediate dependencies: as per Step 2a in Note [Upsweep], these are
          -- the transitive dependencies of the non-boot file which transitively depend
          -- on the boot file.
          Set.filter (\nk -> nodeKeyUnitId nk == uid  -- Cheap test
                              && (NodeKey_Module (key IsBoot)) `Set.member` expectJust "dep_on_boot" (M.lookup nk trans_deps_map)) $
          expectJust "not_boot_dep" (M.lookup (NodeKey_Module (key NotBoot)) trans_deps_map)
          where
            key ib = ModNodeKeyWithUid (GWIB mn ib) uid


        -- An environment mapping a module to its hs-boot file and all nodes on the path between the two, if one exists
        boot_modules = mkModuleEnv
          [ (ms_mod ms, (m, boot_path (ms_mod_name ms) (ms_unitid ms))) | m@(ModuleNode _ ms) <- (mgModSummaries' mod_graph), isBootSummary ms == IsBoot]

        select_boot_modules :: [ModuleGraphNode] -> [ModuleGraphNode]
        select_boot_modules = mapMaybe (fmap fst . get_boot_module)

        get_boot_module :: ModuleGraphNode -> Maybe (ModuleGraphNode, [ModuleGraphNode])
        get_boot_module m = case m of ModuleNode _ ms | HsSrcFile <- ms_hsc_src ms -> lookupModuleEnv boot_modules (ms_mod ms); _ -> Nothing

        -- Any cycles should be resolved now
        collapseSCC :: [SCC ModuleGraphNode] -> Either [ModuleGraphNode] [(Either ModuleGraphNode ModuleGraphNodeWithBootFile)]
        -- Must be at least two nodes, as we were in a cycle
        collapseSCC [AcyclicSCC node1, AcyclicSCC node2] = Right [toNodeWithBoot node1, toNodeWithBoot node2]
        collapseSCC (AcyclicSCC node : nodes) = either (Left . (node :)) (Right . (toNodeWithBoot node :)) (collapseSCC nodes)
        -- Cyclic
        collapseSCC nodes = Left (flattenSCCs nodes)

        toNodeWithBoot :: ModuleGraphNode -> Either ModuleGraphNode ModuleGraphNodeWithBootFile
        toNodeWithBoot mn =
          case get_boot_module mn of
            -- The node doesn't have a boot file
            Nothing -> Left mn
            -- The node does have a boot file
            Just path -> Right (ModuleGraphNodeWithBootFile mn (map mkNodeKey (snd path)))

        -- The toposort and accumulation of acyclic modules is solely to pick-up
        -- hs-boot files which are **not** part of cycles.
        collapseAcyclic :: [SCC ModuleGraphNode] -> [BuildPlan]
        collapseAcyclic (AcyclicSCC node : nodes) = SingleModule node : collapseAcyclic nodes
        collapseAcyclic (CyclicSCC cy_nodes : nodes) = (UnresolvedCycle cy_nodes) : collapseAcyclic nodes
        collapseAcyclic [] = []

        topSortWithBoot nodes = topSortModules False (select_boot_modules nodes ++ nodes) Nothing


  in

    assertPpr (sum (map countMods build_plan) == length (mgModSummaries' mod_graph))
              (vcat [text "Build plan missing nodes:", (text "PLAN:" <+> ppr (sum (map countMods build_plan))), (text "GRAPH:" <+> ppr (length (mgModSummaries' mod_graph )))])
              build_plan

mkWorkerLimit :: DynFlags -> IO WorkerLimit
mkWorkerLimit dflags =
  case parMakeCount dflags of
    Nothing -> pure $ num_procs 1
    Just (ParMakeSemaphore h) -> pure (JSemLimit (SemaphoreName h))
    Just ParMakeNumProcessors -> num_procs <$> getNumProcessors
    Just (ParMakeThisMany n) -> pure $ num_procs n
  where
    num_procs x = NumProcessorsLimit (max 1 x)

isWorkerLimitSequential :: WorkerLimit -> Bool
isWorkerLimitSequential (NumProcessorsLimit x) = x <= 1
isWorkerLimitSequential (JSemLimit {})         = False

-- | This describes what we use to limit the number of jobs, either we limit it
-- ourselves to a specific number or we have an external parallelism semaphore
-- limit it for us.
data WorkerLimit
  = NumProcessorsLimit Int
  | JSemLimit
    SemaphoreName
      -- ^ Semaphore name to use
  deriving Eq

-- | Generalized version of 'load' which also supports a custom
-- 'Messager' (for reporting progress) and 'ModuleGraph' (generally
-- produced by calling 'depanal'.
load' :: GhcMonad m => Maybe ModIfaceCache -> LoadHowMuch -> (GhcMessage -> AnyGhcDiagnostic) -> Maybe Messager -> ModuleGraph -> m SuccessFlag
load' mhmi_cache how_much diag_wrapper mHscMessage mod_graph = do
    -- In normal usage plugins are initialised already by ghc/Main.hs this is protective
    -- for any client who might interact with GHC via load'.
    -- See Note [Timing of plugin initialization]
    initializeSessionPlugins
    modifySession $ \hsc_env -> hsc_env { hsc_mod_graph = mod_graph }
    guessOutputFile
    hsc_env <- getSession

    let dflags = hsc_dflags hsc_env
    let logger = hsc_logger hsc_env
    let interp = hscInterp hsc_env

    -- The "bad" boot modules are the ones for which we have
    -- B.hs-boot in the module graph, but no B.hs
    -- The downsweep should have ensured this does not happen
    -- (see msDeps)
    let all_home_mods =
          Set.fromList [ Module (ms_unitid s) (ms_mod_name s)
                    | s <- mgModSummaries mod_graph, isBootSummary s == NotBoot]
    -- TODO: Figure out what the correct form of this assert is. It's violated
    -- when you have HsBootMerge nodes in the graph: then you'll have hs-boot
    -- files without corresponding hs files.
    --  bad_boot_mods = [s        | s <- mod_graph, isBootSummary s,
    --                              not (ms_mod_name s `elem` all_home_mods)]
    -- assert (null bad_boot_mods ) return ()

    -- check that the module given in HowMuch actually exists, otherwise
    -- topSortModuleGraph will bomb later.
    let checkHowMuch (LoadUpTo m)           = checkMod m
        checkHowMuch (LoadDependenciesOf m) = checkMod m
        checkHowMuch _ = id

        checkMod m and_then
            | m `Set.member` all_home_mods = and_then
            | otherwise = do
                    throwOneError $ mkPlainErrorMsgEnvelope noSrcSpan
                                  $ GhcDriverMessage
                                  $ DriverModuleNotFound (moduleName m)

    checkHowMuch how_much $ do

    -- mg2_with_srcimps drops the hi-boot nodes, returning a
    -- graph with cycles. It is just used for warning about unnecessary source imports.
    let mg2_with_srcimps :: [SCC ModuleGraphNode]
        mg2_with_srcimps = topSortModuleGraph True mod_graph Nothing

    -- If we can determine that any of the {-# SOURCE #-} imports
    -- are definitely unnecessary, then emit a warning.
    warnUnnecessarySourceImports (filterToposortToModules mg2_with_srcimps)

    let maybe_top_mod = case how_much of
                          LoadUpTo m           -> Just m
                          LoadDependenciesOf m -> Just m
                          _                    -> Nothing

        build_plan = createBuildPlan mod_graph maybe_top_mod


    cache <- liftIO $ maybe (return []) iface_clearCache mhmi_cache
    let
        -- prune the HPT so everything is not retained when doing an
        -- upsweep.
        !pruned_cache = pruneCache cache
                            (flattenSCCs (filterToposortToModules  mg2_with_srcimps))


    -- before we unload anything, make sure we don't leave an old
    -- interactive context around pointing to dead bindings.  Also,
    -- write an empty HPT to allow the old HPT to be GC'd.

    let pruneHomeUnitEnv hme = hme { homeUnitEnv_hpt = emptyHomePackageTable }
    setSession $ discardIC $ hscUpdateHUG (unitEnv_map pruneHomeUnitEnv) hsc_env
    hsc_env <- getSession

    -- Unload everything
    liftIO $ unload interp hsc_env

    liftIO $ debugTraceMsg logger 2 (hang (text "Ready for upsweep")
                                    2 (ppr build_plan))

    worker_limit <- liftIO $ mkWorkerLimit dflags

    (upsweep_ok, new_deps) <- withDeferredDiagnostics $ do
      hsc_env <- getSession
      liftIO $ upsweep worker_limit hsc_env mhmi_cache diag_wrapper mHscMessage (toCache pruned_cache) build_plan
    modifySession (addDepsToHscEnv new_deps)
    case upsweep_ok of
      Failed -> loadFinish upsweep_ok
      Succeeded -> do
          liftIO $ debugTraceMsg logger 2 (text "Upsweep completely successful.")
          loadFinish upsweep_ok



-- | Finish up after a load.
loadFinish :: GhcMonad m => SuccessFlag -> m SuccessFlag
-- Empty the interactive context and set the module context to the topmost
-- newly loaded module, or the Prelude if none were loaded.
loadFinish all_ok
  = do modifySession discardIC
       return all_ok


-- | If there is no -o option, guess the name of target executable
-- by using top-level source file name as a base.
guessOutputFile :: GhcMonad m => m ()
guessOutputFile = modifySession $ \env ->
    -- Force mod_graph to avoid leaking env
    let !mod_graph = hsc_mod_graph env
        new_home_graph =
          flip unitEnv_map (hsc_HUG env) $ \hue ->
            let dflags = homeUnitEnv_dflags hue
                platform = targetPlatform dflags
                mainModuleSrcPath :: Maybe String
                mainModuleSrcPath = do
                  ms <- mgLookupModule mod_graph (mainModIs hue)
                  ml_hs_file (ms_location ms)
                name = fmap dropExtension mainModuleSrcPath

                -- MP: This exception is quite sensitive to being forced, if you
                -- force it here then the error message is different because it gets
                -- caught by a different error handler than the test (T9930fail) expects.
                -- Putting an exception into DynFlags is probably not a great design but
                -- I'll write this comment rather than more eagerly force the exception.
                name_exe = do
                  -- we must add the .exe extension unconditionally here, otherwise
                  -- when name has an extension of its own, the .exe extension will
                 -- not be added by GHC.Driver.Pipeline.exeFileName.  See #2248
                 !name' <- case platformArchOS platform of
                             ArchOS _ OSMinGW32  -> fmap (<.> "exe") name
                             ArchOS ArchWasm32 _ -> fmap (<.> "wasm") name
                             _ -> name
                 mainModuleSrcPath' <- mainModuleSrcPath
                 -- #9930: don't clobber input files (unless they ask for it)
                 if name' == mainModuleSrcPath'
                   then throwGhcException . UsageError $
                        "default output name would overwrite the input file; " ++
                        "must specify -o explicitly"
                   else Just name'
            in
              case outputFile_ dflags of
                Just _ -> hue
                Nothing -> hue {homeUnitEnv_dflags = dflags { outputFile_ = name_exe } }
    in env { hsc_unit_env = (hsc_unit_env env) { ue_home_unit_graph = new_home_graph } }

-- -----------------------------------------------------------------------------
--
-- | Prune the HomePackageTable
--
-- Before doing an upsweep, we can throw away:
--
--   - all ModDetails, all linked code
--   - all unlinked code that is out of date with respect to
--     the source file
--
-- This is VERY IMPORTANT otherwise we'll end up requiring 2x the
-- space at the end of the upsweep, because the topmost ModDetails of the
-- old HPT holds on to the entire type environment from the previous
-- compilation.
-- Note [GHC Heap Invariants]
pruneCache :: [CachedIface]
                      -> [ModSummary]
                      -> [HomeModInfo]
pruneCache hpt summ
  = strictMap prune hpt
  where prune (CachedIface { cached_modiface = iface
                           , cached_linkable = linkable
                           }) = HomeModInfo iface emptyModDetails linkable'
          where
           modl = miKey iface
           linkable'
                | Just ms <- M.lookup modl ms_map
                , mi_src_hash iface /= ms_hs_hash ms
                = emptyHomeModInfoLinkable
                | otherwise
                = linkable

        -- Using UFM Module is safe for determinism because the map is just used for a transient lookup. The cache should be unique and a key clash is an error.
        ms_map = M.fromListWith
                  (\ms1 ms2 -> assertPpr False (text "prune_cache" $$ (ppr ms1 <+> ppr ms2))
                               ms2)
                  [(msKey ms, ms) | ms <- summ]

-- ---------------------------------------------------------------------------
--
-- | Unloading
unload :: Interp -> HscEnv -> IO ()
unload interp hsc_env
  = case ghcLink (hsc_dflags hsc_env) of
        LinkInMemory -> Linker.unload interp hsc_env []
        _other -> return ()


{- Parallel Upsweep

The parallel upsweep attempts to concurrently compile the modules in the
compilation graph using multiple Haskell threads.

The Algorithm

* The list of `MakeAction`s are created by `interpretBuildPlan`. A `MakeAction` is
a pair of an `IO a` action and a `MVar a`, where to place the result.
  The list is sorted topologically, so can be executed in order without fear of
  blocking.
* runPipelines takes this list and eventually passes it to runLoop which executes
  each action and places the result into the right MVar.
* The amount of parallelism is controlled by a semaphore. This is just used around the
  module compilation step, so that only the right number of modules are compiled at
  the same time which reduces overall memory usage and allocations.
* Each proper node has a LogQueue, which dictates where to send it's output.
* The LogQueue is placed into the LogQueueQueue when the action starts and a worker
  thread processes the LogQueueQueue printing logs for each module in a stable order.
* The result variable for an action producing `a` is of type `Maybe a`, therefore
  it is still filled on a failure. If a module fails to compile, the
  failure is propagated through the whole module graph and any modules which didn't
  depend on the failure can still be compiled. This behaviour also makes the code
  quite a bit cleaner.
-}


{-

Note [--make mode]
~~~~~~~~~~~~~~~~~
There are two main parts to `--make` mode.

1. `downsweep`: Starts from the top of the module graph and computes dependencies.
2. `upsweep`: Starts from the bottom of the module graph and compiles modules.

The result of the downsweep is a 'ModuleGraph', which is then passed to 'upsweep' which
computers how to build this ModuleGraph.

Note [Upsweep]
~~~~~~~~~~~~~~
Upsweep takes a 'ModuleGraph' as input, computes a build plan and then executes
the plan in order to compile the project.

The first step is computing the build plan from a 'ModuleGraph'.

The output of this step is a `[BuildPlan]`, which is a topologically sorted plan for
how to build all the modules.

```
data BuildPlan = SingleModule ModuleGraphNode  -- A simple, single module all alone but *might* have an hs-boot file which isn't part of a cycle
               | ResolvedCycle [Either ModuleGraphNode ModuleGraphNodeWithBoot]   -- A resolved cycle, linearised by hs-boot files
               | UnresolvedCycle [ModuleGraphNode] -- An actual cycle, which wasn't resolved by hs-boot files
```

The plan is computed in two steps:

Step 1:  Topologically sort the module graph without hs-boot files. This returns a [SCC ModuleGraphNode] which contains
         cycles.
Step 2:  For each cycle, topologically sort the modules in the cycle *with* the relevant hs-boot files. This should
         result in an acyclic build plan if the hs-boot files are sufficient to resolve the cycle.
Step 2a: For each module in the cycle, if the module has a boot file then compute the
         modules on the path between it and the hs-boot file.
         These are the intermediate modules which:
            (1) are (transitive) dependencies of the non-boot module, and
            (2) have the boot module as a (transitive) dependency.
         In particular, all such intermediate modules must appear in the same unit as
         the module under consideration, as module cycles cannot cross unit boundaries.
         This information is stored in ModuleGraphNodeWithBoot.

The `[BuildPlan]` is then interpreted by the `interpretBuildPlan` function.

* SingleModule nodes are compiled normally by either the upsweep_inst or upsweep_mod functions.
* ResolvedCycles need to compiled "together" so that modules outside the cycle are presented
  with a consistent knot-tied version of modules at the end.
    - When the ModuleGraphNodeWithBoot nodes are compiled then suitable rehydration
      is performed both before and after the module in question is compiled.
      See Note [Hydrating Modules] for more information.
* UnresolvedCycles are indicative of a proper cycle, unresolved by hs-boot files
  and are reported as an error to the user.

The main trickiness of `interpretBuildPlan` is deciding which version of a dependency
is visible from each module. For modules which are not in a cycle, there is just
one version of a module, so that is always used. For modules in a cycle, there are two versions of
'HomeModInfo'.

1. Internal to loop: The version created whilst compiling the loop by upsweep_mod.
2. External to loop: The knot-tied version created by typecheckLoop.

Whilst compiling a module inside the loop, we need to use the (1). For a module which
is outside of the loop which depends on something from in the loop, the (2) version
is used.

As the plan is interpreted, which version of a HomeModInfo is visible is updated
by updating a map held in a state monad. So after a loop has finished being compiled,
the visible module is the one created by typecheckLoop and the internal version is not
used again.

This plan also ensures the most important invariant to do with module loops:

> If you depend on anything within a module loop, before you can use the dependency,
  the whole loop has to finish compiling.

The end result of `interpretBuildPlan` is a `[MakeAction]`, which are pairs
of `IO a` actions and a `MVar (Maybe a)`, somewhere to put the result of running
the action. This list is topologically sorted, so can be run in order to compute
the whole graph.

As well as this `interpretBuildPlan` also outputs an `IO [Maybe (Maybe HomeModInfo)]` which
can be queried at the end to get the result of all modules at the end, with their proper
visibility. For example, if any module in a loop fails then all modules in that loop will
report as failed because the visible node at the end will be the result of checking
these modules together.

-}

-- | Simple wrapper around MVar which allows a functor instance.
data ResultVar b = forall a . ResultVar (a -> b) (MVar (Maybe a))

deriving instance Functor ResultVar

mkResultVar :: MVar (Maybe a) -> ResultVar a
mkResultVar = ResultVar id

-- | Block until the result is ready.
waitResult :: ResultVar a -> MaybeT IO a
waitResult (ResultVar f var) = MaybeT (fmap f <$> readMVar var)

data BuildResult = BuildResult { _resultOrigin :: ResultOrigin
                               , resultVar    :: ResultVar (Maybe HomeModInfo, ModuleNameSet)
                               }

-- The origin of this result var, useful for debugging
data ResultOrigin = NoLoop | Loop ResultLoopOrigin deriving (Show)

data ResultLoopOrigin = Initialise | Rehydrated | Finalised deriving (Show)

mkBuildResult :: ResultOrigin -> ResultVar (Maybe HomeModInfo, ModuleNameSet) -> BuildResult
mkBuildResult = BuildResult


data BuildLoopState = BuildLoopState { buildDep :: M.Map NodeKey BuildResult
                                          -- The current way to build a specific TNodeKey, without cycles this just points to
                                          -- the appropriate result of compiling a module  but with
                                          -- cycles there can be additional indirection and can point to the result of typechecking a loop
                                     , nNODE :: Int
                                     , hug_var :: MVar HomeUnitGraph
                                     -- A global variable which is incrementally updated with the result
                                     -- of compiling modules.
                                     }

nodeId :: BuildM Int
nodeId = do
  n <- gets nNODE
  modify (\m -> m { nNODE = n + 1 })
  return n


setModulePipeline :: NodeKey -> BuildResult -> BuildM ()
setModulePipeline mgn build_result = do
  modify (\m -> m { buildDep = M.insert mgn build_result (buildDep m) })

type BuildMap = M.Map NodeKey BuildResult

getBuildMap :: BuildM BuildMap
getBuildMap = gets buildDep

getDependencies :: [NodeKey] -> BuildMap -> [BuildResult]
getDependencies direct_deps build_map =
  strictMap (expectJust "dep_map" . flip M.lookup build_map) direct_deps

type BuildM a = StateT BuildLoopState IO a




-- | Environment used when compiling a module
data MakeEnv = MakeEnv { hsc_env :: !HscEnv -- The basic HscEnv which will be augmented for each module
                       , compile_sem :: !AbstractSem
                       -- Modify the environment for module k, with the supplied logger modification function.
                       -- For -j1, this wrapper doesn't do anything
                       -- For -jn, the wrapper initialised a log queue and then modifies the logger to pipe its output
                       --          into the log queue.
                       , withLogger :: forall a . Int -> ((Logger -> Logger) -> IO a) -> IO a
                       , env_messager :: !(Maybe Messager)
                       , diag_wrapper :: GhcMessage -> AnyGhcDiagnostic
                       }

type RunMakeM a = ReaderT MakeEnv (MaybeT IO) a

-- | Given the build plan, creates a graph which indicates where each NodeKey should
-- get its direct dependencies from. This might not be the corresponding build action
-- if the module participates in a loop. This step also labels each node with a number for the output.
-- See Note [Upsweep] for a high-level description.
interpretBuildPlan :: HomeUnitGraph
                   -> Maybe ModIfaceCache
                   -> M.Map ModNodeKeyWithUid HomeModInfo
                   -> [BuildPlan]
                   -> IO ( Maybe [ModuleGraphNode] -- Is there an unresolved cycle
                         , [MakeAction] -- Actions we need to run in order to build everything
                         , IO [Maybe (Maybe HomeModInfo)]) -- An action to query to get all the built modules at the end.
interpretBuildPlan hug mhmi_cache old_hpt plan = do
  hug_var <- newMVar hug
  ((mcycle, plans), build_map) <- runStateT (buildLoop plan) (BuildLoopState M.empty 1 hug_var)
  let wait = collect_results (buildDep build_map)
  return (mcycle, plans, wait)

  where
    collect_results build_map =
      sequence (map (\br -> collect_result (fst <$> resultVar br)) (M.elems build_map))
      where
        collect_result res_var = runMaybeT (waitResult res_var)

    n_mods = sum (map countMods plan)

    buildLoop :: [BuildPlan]
              -> BuildM (Maybe [ModuleGraphNode], [MakeAction])
    -- Build the abstract pipeline which we can execute
    -- Building finished
    buildLoop []           = return (Nothing, [])
    buildLoop (plan:plans) =
      case plan of
        -- If there was no cycle, then typecheckLoop is not necessary
        SingleModule m -> do
          one_plan <- buildSingleModule Nothing NoLoop m
          (cycle, all_plans) <- buildLoop plans
          return (cycle, one_plan : all_plans)

        -- For a resolved cycle, depend on everything in the loop, then update
        -- the cache to point to this node rather than directly to the module build
        -- nodes
        ResolvedCycle ms -> do
          pipes <- buildModuleLoop ms
          (cycle, graph) <- buildLoop plans
          return (cycle, pipes ++ graph)

        -- Can't continue past this point as the cycle is unresolved.
        UnresolvedCycle ns -> return (Just ns, [])

    buildSingleModule :: Maybe [NodeKey]  -- Modules we need to rehydrate before compiling this module
                      -> ResultOrigin
                      -> ModuleGraphNode          -- The node we are compiling
                      -> BuildM MakeAction
    buildSingleModule rehydrate_nodes origin mod = do
      mod_idx <- nodeId
      !build_map <- getBuildMap
      hug_var <- gets hug_var
      -- 1. Get the direct dependencies of this module
      let direct_deps = nodeDependencies False mod
          -- It's really important to force build_deps, or the whole buildMap is retained,
          -- which would retain all the result variables, preventing us from collecting them
          -- after they are no longer used.
          !build_deps = getDependencies direct_deps build_map
      let !build_action =
            case mod of
              InstantiationNode uid iu -> do
                withCurrentUnit (moduleGraphNodeUnitId mod) $ do
                  (hug, deps) <- wait_deps_hug hug_var build_deps
                  executeInstantiationNode mod_idx n_mods hug uid iu
                  return (Nothing, deps)
              ModuleNode _build_deps ms ->
                let !old_hmi = M.lookup (msKey ms) old_hpt
                    rehydrate_mods = mapMaybe nodeKeyModName <$> rehydrate_nodes
                in withCurrentUnit (moduleGraphNodeUnitId mod) $ do
                     (hug, deps) <- wait_deps_hug hug_var build_deps
                     hmi <- executeCompileNode mod_idx n_mods old_hmi hug rehydrate_mods ms
                     -- Write the HMI to an external cache (if one exists)
                     -- See Note [Caching HomeModInfo]
                     liftIO $ forM mhmi_cache $ \hmi_cache -> addHmiToCache hmi_cache hmi
                     -- This global MVar is incrementally modified in order to avoid having to
                     -- recreate the HPT before compiling each module which leads to a quadratic amount of work.
                     liftIO $ modifyMVar_ hug_var (return . addHomeModInfoToHug hmi)
                     return (Just hmi, addToModuleNameSet (moduleGraphNodeUnitId mod) (ms_mod_name ms) deps )
              LinkNode _nks uid -> do
                  withCurrentUnit (moduleGraphNodeUnitId mod) $ do
                    (hug, deps) <- wait_deps_hug hug_var build_deps
                    executeLinkNode hug (mod_idx, n_mods) uid direct_deps
                    return (Nothing, deps)


      res_var <- liftIO newEmptyMVar
      let result_var = mkResultVar res_var
      setModulePipeline (mkNodeKey mod) (mkBuildResult origin result_var)
      return $! (MakeAction build_action res_var)


    buildOneLoopyModule :: ModuleGraphNodeWithBootFile -> BuildM [MakeAction]
    buildOneLoopyModule (ModuleGraphNodeWithBootFile mn deps) = do
      ma <- buildSingleModule (Just deps) (Loop Initialise) mn
      -- Rehydration (1) from Note [Hydrating Modules], "Loops with multiple boot files"
      rehydrate_action <- rehydrateAction Rehydrated ((GWIB (mkNodeKey mn) IsBoot) : (map (\d -> GWIB d NotBoot) deps))
      return $ [ma, rehydrate_action]


    buildModuleLoop :: [Either ModuleGraphNode ModuleGraphNodeWithBootFile] -> BuildM [MakeAction]
    buildModuleLoop ms = do
      build_modules <- concatMapM (either (fmap (:[]) <$> buildSingleModule Nothing (Loop Initialise)) buildOneLoopyModule) ms
      let extract (Left mn) = GWIB (mkNodeKey mn) NotBoot
          extract (Right (ModuleGraphNodeWithBootFile mn _)) = GWIB (mkNodeKey mn) IsBoot
      let loop_mods = map extract ms
      -- Rehydration (2) from Note [Hydrating Modules], "Loops with multiple boot files"
      -- Fixes the space leak described in that note.
      rehydrate_action <- rehydrateAction Finalised loop_mods

      return $ build_modules ++ [rehydrate_action]

    -- An action which rehydrates the given keys
    rehydrateAction :: ResultLoopOrigin -> [GenWithIsBoot NodeKey] -> BuildM MakeAction
    rehydrateAction origin deps = do
      hug_var <- gets hug_var
      !build_map <- getBuildMap
      res_var <- liftIO newEmptyMVar
      let loop_unit :: UnitId
          !loop_unit = nodeKeyUnitId (gwib_mod (head deps))
          !build_deps = getDependencies (map gwib_mod deps) build_map
      let loop_action = withCurrentUnit loop_unit $ do
            (hug, tdeps) <- wait_deps_hug hug_var build_deps
            hsc_env <- asks hsc_env
            let new_hsc = setHUG hug hsc_env
                mns :: [ModuleName]
                mns = mapMaybe (nodeKeyModName . gwib_mod) deps

            hmis' <- liftIO $ rehydrateAfter new_hsc mns

            checkRehydrationInvariant hmis' deps

            -- Add hydrated interfaces to global variable
            liftIO $ modifyMVar_ hug_var (\hug -> return $ foldr addHomeModInfoToHug hug hmis')
            return (hmis', tdeps)

      let fanout i = first (Just . (!! i)) <$> mkResultVar res_var
      -- From outside the module loop, anyone must wait for the loop to finish and then
      -- use the result of the rehydrated iface. This makes sure that things not in the
      -- module loop will see the updated interfaces for all the identifiers in the loop.
          boot_key :: NodeKey -> NodeKey
          boot_key (NodeKey_Module m) = NodeKey_Module (m { mnkModuleName = (mnkModuleName m) { gwib_isBoot = IsBoot } } )
          boot_key k = pprPanic "boot_key" (ppr k)

          update_module_pipeline (m, i) =
            case gwib_isBoot m of
              NotBoot -> setModulePipeline (gwib_mod m) (mkBuildResult (Loop origin) (fanout i))
              IsBoot -> do
                setModulePipeline (gwib_mod m) (mkBuildResult (Loop origin) (fanout i))
                -- SPECIAL: Anything outside the loop needs to see A rather than A.hs-boot
                setModulePipeline (boot_key (gwib_mod m)) (mkBuildResult (Loop origin) (fanout i))

      let deps_i = zip deps [0..]
      mapM update_module_pipeline deps_i

      return $ MakeAction loop_action res_var

      -- Checks that the interfaces returned from hydration match-up with the names of the
      -- modules which were fed into the function.
    checkRehydrationInvariant hmis deps =
        let hmi_names = map (moduleName . mi_module . hm_iface) hmis
            start = mapMaybe (nodeKeyModName . gwib_mod) deps
        in massertPpr (hmi_names == start) $ (ppr hmi_names $$ ppr start)


withCurrentUnit :: UnitId -> RunMakeM a -> RunMakeM a
withCurrentUnit uid = do
  local (\env -> env { hsc_env = hscSetActiveUnitId uid (hsc_env env)})

upsweep
    :: WorkerLimit -- ^ The number of workers we wish to run in parallel
    -> HscEnv -- ^ The base HscEnv, which is augmented for each module
    -> Maybe ModIfaceCache -- ^ A cache to incrementally write final interface files to
    -> (GhcMessage -> AnyGhcDiagnostic)
    -> Maybe Messager
    -> M.Map ModNodeKeyWithUid HomeModInfo
    -> [BuildPlan]
    -> IO (SuccessFlag, [HomeModInfo])
upsweep n_jobs hsc_env hmi_cache diag_wrapper mHscMessage old_hpt build_plan = do
    (cycle, pipelines, collect_result) <- interpretBuildPlan (hsc_HUG hsc_env) hmi_cache old_hpt build_plan
    runPipelines n_jobs hsc_env diag_wrapper mHscMessage pipelines
    res <- collect_result

    let completed = [m | Just (Just m) <- res]

    -- Handle any cycle in the original compilation graph and return the result
    -- of the upsweep.
    case cycle of
        Just mss -> do
          throwOneError $ cyclicModuleErr mss
        Nothing  -> do
          let success_flag = successIf (all isJust res)
          return (success_flag, completed)

toCache :: [HomeModInfo] -> M.Map (ModNodeKeyWithUid) HomeModInfo
toCache hmis = M.fromList ([(miKey $ hm_iface hmi, hmi) | hmi <- hmis])

miKey :: ModIface -> ModNodeKeyWithUid
miKey hmi = ModNodeKeyWithUid (mi_mnwib hmi) ((toUnitId $ moduleUnit (mi_module hmi)))

upsweep_inst :: HscEnv
             -> Maybe Messager
             -> Int  -- index of module
             -> Int  -- total number of modules
             -> UnitId
             -> InstantiatedUnit
             -> IO ()
upsweep_inst hsc_env mHscMessage mod_index nmods uid iuid = do
        case mHscMessage of
            Just hscMessage -> hscMessage hsc_env (mod_index, nmods) (NeedsRecompile MustCompile) (InstantiationNode uid iuid)
            Nothing -> return ()
        runHsc hsc_env $ ioMsgMaybe $ hoistTcRnMessage $ tcRnCheckUnit hsc_env $ VirtUnit iuid
        pure ()

-- | Compile a single module.  Always produce a Linkable for it if
-- successful.  If no compilation happened, return the old Linkable.
upsweep_mod :: HscEnv
            -> Maybe Messager
            -> Maybe HomeModInfo
            -> ModSummary
            -> Int  -- index of module
            -> Int  -- total number of modules
            -> IO HomeModInfo
upsweep_mod hsc_env mHscMessage old_hmi summary mod_index nmods =  do
  hmi <- compileOne' mHscMessage hsc_env summary
          mod_index nmods (hm_iface <$> old_hmi) (maybe emptyHomeModInfoLinkable hm_linkable old_hmi)

  -- MP: This is a bit janky, because before you add the entries you have to extend the HPT with the module
  -- you just compiled. Another option, would be delay adding anything until after upsweep has finished, but I
  -- am unsure if this is sound (wrt running TH splices for example).
  -- This function only does anything if the linkable produced is a BCO, which
  -- used to only happen with the bytecode backend, but with
  -- @-fprefer-byte-code@, @HomeModInfo@ has bytecode even when generating
  -- object code, see #25230.
  addSptEntries (hscUpdateHPT (\hpt -> addToHpt hpt (ms_mod_name summary) hmi) hsc_env)
                (homeModInfoByteCode hmi)

  return hmi

-- | Add the entries from a BCO linkable to the SPT table, see
-- See Note [Grand plan for static forms] in GHC.Iface.Tidy.StaticPtrTable.
addSptEntries :: HscEnv -> Maybe Linkable -> IO ()
addSptEntries hsc_env mlinkable =
  hscAddSptEntries hsc_env
     [ spt
     | linkable <- maybeToList mlinkable
     , bco <- linkableBCOs linkable
     , spt <- bc_spt_entries bco
     ]

{- Note [-fno-code mode]
~~~~~~~~~~~~~~~~~~~~~~~~
GHC offers the flag -fno-code for the purpose of parsing and typechecking a
program without generating object files. This is intended to be used by tooling
and IDEs to provide quick feedback on any parser or type errors as cheaply as
possible.

When GHC is invoked with -fno-code no object files or linked output will be
generated. As many errors and warnings as possible will be generated, as if
-fno-code had not been passed. The session DynFlags will have
backend == NoBackend.

-fwrite-interface
~~~~~~~~~~~~~~~~
Whether interface files are generated in -fno-code mode is controlled by the
-fwrite-interface flag. The -fwrite-interface flag is a no-op if -fno-code is
not also passed. Recompilation avoidance requires interface files, so passing
-fno-code without -fwrite-interface should be avoided. If -fno-code were
re-implemented today, -fwrite-interface would be discarded and it would be
considered always on; this behaviour is as it is for backwards compatibility.

================================================================
IN SUMMARY: ALWAYS PASS -fno-code AND -fwrite-interface TOGETHER
================================================================

Template Haskell
~~~~~~~~~~~~~~~~
A module using template haskell may invoke an imported function from inside a
splice. This will cause the type-checker to attempt to execute that code, which
would fail if no object files had been generated. See #8025. To rectify this,
during the downsweep we patch the DynFlags in the ModSummary of any home module
that is imported by a module that uses template haskell, to generate object
code.

The flavour of the generated code depends on whether `-fprefer-byte-code` is enabled
or not in the module which needs the code generation. If the module requires byte-code then
dependencies will generate byte-code, otherwise they will generate object files.
In the case where some modules require byte-code and some object files, both are
generated by enabling `-fbyte-code-and-object-code`, the test "fat015" tests these
configurations.

The object files (and interface files if -fwrite-interface is disabled) produced
for template haskell are written to temporary files.

Note that since template haskell can run arbitrary IO actions, -fno-code mode
is no more secure than running without it.

Potential TODOS:
~~~~~
* Remove -fwrite-interface and have interface files always written in -fno-code
  mode
* Both .o and .dyn_o files are generated for template haskell, but we only need
  .dyn_o. Fix it.
* In make mode, a message like
  Compiling A (A.hs, /tmp/ghc_123.o)
  is shown if downsweep enabled object code generation for A. Perhaps we should
  show "nothing" or "temporary object file" instead. Note that one
  can currently use -keep-tmp-files and inspect the generated file with the
  current behaviour.
* Offer a -no-codedir command line option, and write what were temporary
  object files there. This would speed up recompilation.
* Use existing object files (if they are up to date) instead of always
  generating temporary ones.
-}

-- Note [When source is considered modified]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A number of functions in GHC.Driver accept a SourceModified argument, which
-- is part of how GHC determines whether recompilation may be avoided (see the
-- definition of the SourceModified data type for details).
--
-- Determining whether or not a source file is considered modified depends not
-- only on the source file itself, but also on the output files which compiling
-- that module would produce. This is done because GHC supports a number of
-- flags which control which output files should be produced, e.g. -fno-code
-- -fwrite-interface and -fwrite-ide-file; we must check not only whether the
-- source file has been modified since the last compile, but also whether the
-- source file has been modified since the last compile which produced all of
-- the output files which have been requested.
--
-- Specifically, a source file is considered unmodified if it is up-to-date
-- relative to all of the output files which have been requested. Whether or
-- not an output file is up-to-date depends on what kind of file it is:
--
-- * iface (.hi) files are considered up-to-date if (and only if) their
--   mi_src_hash field matches the hash of the source file,
--
-- * all other output files (.o, .dyn_o, .hie, etc) are considered up-to-date
--   if (and only if) their modification times on the filesystem are greater
--   than or equal to the modification time of the corresponding .hi file.
--
-- Why do we use '>=' rather than '>' for output files other than the .hi file?
-- If the filesystem has poor resolution for timestamps (e.g. FAT32 has a
-- resolution of 2 seconds), we may often find that the .hi and .o files have
-- the same modification time. Using >= is slightly unsafe, but it matches
-- make's behaviour.
--
-- This strategy allows us to do the minimum work necessary in order to ensure
-- that all the files the user cares about are up-to-date; e.g. we should not
-- worry about .o files if the user has indicated that they are not interested
-- in them via -fno-code. See also #9243.
--
-- Note that recompilation avoidance is dependent on .hi files being produced,
-- which does not happen if -fno-write-interface -fno-code is passed. That is,
-- passing -fno-write-interface -fno-code means that you cannot benefit from
-- recompilation avoidance. See also Note [-fno-code mode].
--
-- The correctness of this strategy depends on an assumption that whenever we
-- are producing multiple output files, the .hi file is always written first.
-- If this assumption is violated, we risk recompiling unnecessarily by
-- incorrectly regarding non-.hi files as outdated.
--

-- ---------------------------------------------------------------------------
--
-- | Topological sort of the module graph
topSortModuleGraph
          :: Bool
          -- ^ Drop hi-boot nodes? (see below)
          -> ModuleGraph
          -> Maybe HomeUnitModule
             -- ^ Root module name.  If @Nothing@, use the full graph.
          -> [SCC ModuleGraphNode]
-- ^ Calculate SCCs of the module graph, possibly dropping the hi-boot nodes
-- The resulting list of strongly-connected-components is in topologically
-- sorted order, starting with the module(s) at the bottom of the
-- dependency graph (ie compile them first) and ending with the ones at
-- the top.
--
-- Drop hi-boot nodes (first boolean arg)?
--
-- - @False@:   treat the hi-boot summaries as nodes of the graph,
--              so the graph must be acyclic
--
-- - @True@:    eliminate the hi-boot nodes, and instead pretend
--              the a source-import of Foo is an import of Foo
--              The resulting graph has no hi-boot nodes, but can be cyclic
topSortModuleGraph drop_hs_boot_nodes module_graph mb_root_mod =
    -- stronglyConnCompG flips the original order, so if we reverse
    -- the summaries we get a stable topological sort.
  topSortModules drop_hs_boot_nodes (reverse $ mgModSummaries' module_graph) mb_root_mod

topSortModules :: Bool -> [ModuleGraphNode] -> Maybe HomeUnitModule -> [SCC ModuleGraphNode]
topSortModules drop_hs_boot_nodes summaries mb_root_mod
  = map (fmap summaryNodeSummary) $ stronglyConnCompG initial_graph
  where
    (graph, lookup_node) =
      moduleGraphNodes drop_hs_boot_nodes summaries

    initial_graph = case mb_root_mod of
        Nothing -> graph
        Just (Module uid root_mod) ->
            -- restrict the graph to just those modules reachable from
            -- the specified module.  We do this by building a graph with
            -- the full set of nodes, and determining the reachable set from
            -- the specified node.
            let root | Just node <- lookup_node $ NodeKey_Module $ ModNodeKeyWithUid (GWIB root_mod NotBoot) uid
                     , graph `hasVertexG` node
                     = node
                     | otherwise
                     = throwGhcException (ProgramError "module does not exist")
            in graphFromEdgedVerticesUniq (seq root (reachableG graph root))

newtype ModNodeMap a = ModNodeMap { unModNodeMap :: Map.Map ModNodeKey a }
  deriving (Functor, Traversable, Foldable)

emptyModNodeMap :: ModNodeMap a
emptyModNodeMap = ModNodeMap Map.empty

modNodeMapInsert :: ModNodeKey -> a -> ModNodeMap a -> ModNodeMap a
modNodeMapInsert k v (ModNodeMap m) = ModNodeMap (Map.insert k v m)

modNodeMapElems :: ModNodeMap a -> [a]
modNodeMapElems (ModNodeMap m) = Map.elems m

modNodeMapLookup :: ModNodeKey -> ModNodeMap a -> Maybe a
modNodeMapLookup k (ModNodeMap m) = Map.lookup k m

modNodeMapSingleton :: ModNodeKey -> a -> ModNodeMap a
modNodeMapSingleton k v = ModNodeMap (M.singleton k v)

modNodeMapUnionWith :: (a -> a -> a) -> ModNodeMap a -> ModNodeMap a -> ModNodeMap a
modNodeMapUnionWith f (ModNodeMap m) (ModNodeMap n) = ModNodeMap (M.unionWith f m n)

-- | If there are {-# SOURCE #-} imports between strongly connected
-- components in the topological sort, then those imports can
-- definitely be replaced by ordinary non-SOURCE imports: if SOURCE
-- were necessary, then the edge would be part of a cycle.
warnUnnecessarySourceImports :: GhcMonad m => [SCC ModSummary] -> m ()
warnUnnecessarySourceImports sccs = do
  diag_opts <- initDiagOpts <$> getDynFlags
  when (diag_wopt Opt_WarnUnusedImports diag_opts) $ do
    let check ms =
           let mods_in_this_cycle = map ms_mod_name ms in
           [ warn i | m <- ms, i <- ms_home_srcimps m,
                      unLoc i `notElem`  mods_in_this_cycle ]

        warn :: Located ModuleName -> MsgEnvelope GhcMessage
        warn (L loc mod) = GhcDriverMessage <$> mkPlainMsgEnvelope diag_opts
                                                  loc (DriverUnnecessarySourceImports mod)
    logDiagnostics (mkMessages $ listToBag (concatMap (check . flattenSCC) sccs))


-- This caches the answer to the question, if we are in this unit, what does
-- an import of this module mean.
type DownsweepCache = M.Map (UnitId, PkgQual, ModuleNameWithIsBoot) [Either DriverMessages ModSummary]

-----------------------------------------------------------------------------
--
-- | Downsweep (dependency analysis)
--
-- Chase downwards from the specified root set, returning summaries
-- for all home modules encountered.  Only follow source-import
-- links.
--
-- We pass in the previous collection of summaries, which is used as a
-- cache to avoid recalculating a module summary if the source is
-- unchanged.
--
-- The returned list of [ModSummary] nodes has one node for each home-package
-- module, plus one for any hs-boot files.  The imports of these nodes
-- are all there, including the imports of non-home-package modules.
downsweep :: HscEnv
          -> (GhcMessage -> AnyGhcDiagnostic)
          -> Maybe Messager
          -> [ModSummary]
          -- ^ Old summaries
          -> [ModuleName]       -- Ignore dependencies on these; treat
                                -- them as if they were package modules
          -> Bool               -- True <=> allow multiple targets to have
                                --          the same module name; this is
                                --          very useful for ghc -M
          -> IO ([DriverMessages], [ModuleGraphNode])
                -- The non-error elements of the returned list all have distinct
                -- (Modules, IsBoot) identifiers, unless the Bool is true in
                -- which case there can be repeats
downsweep hsc_env diag_wrapper msg old_summaries excl_mods allow_dup_roots = do
  n_jobs <- mkWorkerLimit (hsc_dflags hsc_env)
  new <- rootSummariesParallel n_jobs hsc_env diag_wrapper msg summary
  downsweep_imports hsc_env old_summary_map excl_mods allow_dup_roots new
  where
    summary = getRootSummary excl_mods old_summary_map

    -- A cache from file paths to the already summarised modules. The same file
    -- can be used in multiple units so the map is also keyed by which unit the
    -- file was used in.
    -- Reuse these if we can because the most expensive part of downsweep is
    -- reading the headers.
    old_summary_map :: M.Map (UnitId, FilePath) ModSummary
    old_summary_map =
      M.fromList [((ms_unitid ms, msHsFilePath ms), ms) | ms <- old_summaries]

downsweep_imports :: HscEnv
                  -> M.Map (UnitId, FilePath) ModSummary
                  -> [ModuleName]
                  -> Bool
                  -> ([(UnitId, DriverMessages)], [ModSummary])
                  -> IO ([DriverMessages], [ModuleGraphNode])
downsweep_imports hsc_env old_summaries excl_mods allow_dup_roots (root_errs, rootSummariesOk)
   = do
       let root_map = mkRootMap rootSummariesOk
       checkDuplicates root_map
       (deps, map0) <- loopSummaries rootSummariesOk (M.empty, root_map)
       let closure_errs = checkHomeUnitsClosed unit_env
           unit_env = hsc_unit_env hsc_env
           tmpfs    = hsc_tmpfs    hsc_env

           downsweep_errs = lefts $ concat $ M.elems map0
           downsweep_nodes = M.elems deps

           (other_errs, unit_nodes) = partitionEithers $ unitEnv_foldWithKey (\nodes uid hue -> nodes ++ unitModuleNodes downsweep_nodes uid hue) [] (hsc_HUG hsc_env)
           all_nodes = downsweep_nodes ++ unit_nodes
           all_errs  = all_root_errs ++  downsweep_errs ++ other_errs
           all_root_errs =  closure_errs ++ map snd root_errs

       -- if we have been passed -fno-code, we enable code generation
       -- for dependencies of modules that have -XTemplateHaskell,
       -- otherwise those modules will fail to compile.
       -- See Note [-fno-code mode] #8025
       th_enabled_nodes <- enableCodeGenForTH logger tmpfs unit_env all_nodes
       if null all_root_errs
         then return (all_errs, th_enabled_nodes)
         else pure $ (all_root_errs, [])
     where
        -- Dependencies arising on a unit (backpack and module linking deps)
        unitModuleNodes :: [ModuleGraphNode] -> UnitId -> HomeUnitEnv -> [Either (Messages DriverMessage) ModuleGraphNode]
        unitModuleNodes summaries uid hue =
          let instantiation_nodes = instantiationNodes uid (homeUnitEnv_units hue)
          in map Right instantiation_nodes
              ++ maybeToList (linkNodes (instantiation_nodes ++ summaries) uid hue)

        calcDeps ms =
          -- Add a dependency on the HsBoot file if it exists
          -- This gets passed to the loopImports function which just ignores it if it
          -- can't be found.
          [(ms_unitid ms, NoPkgQual, GWIB (noLoc $ ms_mod_name ms) IsBoot) | NotBoot <- [isBootSummary ms] ] ++
          [(ms_unitid ms, b, c) | (b, c) <- msDeps ms ]

        logger = hsc_logger hsc_env

        -- In a root module, the filename is allowed to diverge from the module
        -- name, so we have to check that there aren't multiple root files
        -- defining the same module (otherwise the duplicates will be silently
        -- ignored, leading to confusing behaviour).
        checkDuplicates
          :: DownsweepCache
          -> IO ()
        checkDuplicates root_map
           | allow_dup_roots = return ()
           | null dup_roots  = return ()
           | otherwise       = liftIO $ multiRootsErr (head dup_roots)
           where
             dup_roots :: [[ModSummary]]        -- Each at least of length 2
             dup_roots = filterOut isSingleton $ map rights (M.elems root_map)

        -- This loops over all the mod summaries in the dependency graph, accumulates the actual dependencies for each module/unit
        loopSummaries :: [ModSummary]
              -> (M.Map NodeKey ModuleGraphNode,
                    DownsweepCache)
              -> IO ((M.Map NodeKey ModuleGraphNode), DownsweepCache)
        loopSummaries [] done = return done
        loopSummaries (ms:next) (done, summarised)
          | Just {} <- M.lookup k done
          = loopSummaries next (done, summarised)
          -- Didn't work out what the imports mean yet, now do that.
          | otherwise = do
             (final_deps, done', summarised') <- loopImports (calcDeps ms) done summarised
             -- This has the effect of finding a .hs file if we are looking at the .hs-boot file.
             (_, done'', summarised'') <- loopImports (maybeToList hs_file_for_boot) done' summarised'
             loopSummaries next (M.insert k (ModuleNode final_deps ms) done'', summarised'')
          where
            k = NodeKey_Module (msKey ms)

            hs_file_for_boot
              | HsBootFile <- ms_hsc_src ms
              = Just $ ((ms_unitid ms), NoPkgQual, (GWIB (noLoc $ ms_mod_name ms) NotBoot))
              | otherwise
              = Nothing


        -- This loops over each import in each summary. It is mutually recursive with loopSummaries if we discover
        -- a new module by doing this.
        loopImports :: [(UnitId, PkgQual, GenWithIsBoot (Located ModuleName))]
                        -- Work list: process these modules
             -> M.Map NodeKey ModuleGraphNode
             -> DownsweepCache
                        -- Visited set; the range is a list because
                        -- the roots can have the same module names
                        -- if allow_dup_roots is True
             -> IO ([NodeKey],
                  M.Map NodeKey ModuleGraphNode, DownsweepCache)
                        -- The result is the completed NodeMap
        loopImports [] done summarised = return ([], done, summarised)
        loopImports ((home_uid,mb_pkg, gwib) : ss) done summarised
          | Just summs <- M.lookup cache_key summarised
          = case summs of
              [Right ms] -> do
                let nk = NodeKey_Module (msKey ms)
                (rest, summarised', done') <- loopImports ss done summarised
                return (nk: rest, summarised', done')
              [Left _err] ->
                loopImports ss done summarised
              _errs ->  do
                loopImports ss done summarised
          | otherwise
          = do
               mb_s <- summariseModule hsc_env home_unit old_summaries
                                       is_boot wanted_mod mb_pkg
                                       Nothing excl_mods
               case mb_s of
                   NotThere -> loopImports ss done summarised
                   External _ -> do
                    (other_deps, done', summarised') <- loopImports ss done summarised
                    return (other_deps, done', summarised')
                   FoundInstantiation iud -> do
                    (other_deps, done', summarised') <- loopImports ss done summarised
                    return (NodeKey_Unit iud : other_deps, done', summarised')
                   FoundHomeWithError (_uid, e) ->  loopImports ss done (Map.insert cache_key [(Left e)] summarised)
                   FoundHome s -> do
                     (done', summarised') <-
                       loopSummaries [s] (done, Map.insert cache_key [Right s] summarised)
                     (other_deps, final_done, final_summarised) <- loopImports ss done' summarised'

                     -- MP: This assumes that we can only instantiate non home units, which is probably fair enough for now.
                     return (NodeKey_Module (msKey s) : other_deps, final_done, final_summarised)
          where
            cache_key = (home_uid, mb_pkg, unLoc <$> gwib)
            home_unit = ue_unitHomeUnit home_uid (hsc_unit_env hsc_env)
            GWIB { gwib_mod = L loc mod, gwib_isBoot = is_boot } = gwib
            wanted_mod = L loc mod

getRootSummary ::
  [ModuleName] ->
  M.Map (UnitId, FilePath) ModSummary ->
  HscEnv ->
  Target ->
  IO (Either (UnitId, DriverMessages) ModSummary)
getRootSummary excl_mods old_summary_map hsc_env target
  | TargetFile file mb_phase <- targetId
  = do
    let offset_file = augmentByWorkingDirectory dflags file
    exists <- liftIO $ doesFileExist offset_file
    if exists || isJust maybe_buf
    then first (uid,) <$>
         summariseFile hsc_env home_unit old_summary_map offset_file mb_phase
         maybe_buf
    else
      return $ Left $ (uid,) $ singleMessage $
      mkPlainErrorMsgEnvelope noSrcSpan (DriverFileNotFound offset_file)
  | TargetModule modl <- targetId
  = do
    maybe_summary <- summariseModule hsc_env home_unit old_summary_map NotBoot
                     (L rootLoc modl) (ThisPkg (homeUnitId home_unit))
                     maybe_buf excl_mods
    pure case maybe_summary of
      FoundHome s  -> Right s
      FoundHomeWithError err -> Left err
      _ -> Left (uid, moduleNotFoundErr modl)
    where
      Target {targetId, targetContents = maybe_buf, targetUnitId = uid} = target
      home_unit = ue_unitHomeUnit uid (hsc_unit_env hsc_env)
      rootLoc = mkGeneralSrcSpan (fsLit "<command line>")
      dflags = homeUnitEnv_dflags (ue_findHomeUnitEnv uid (hsc_unit_env hsc_env))

-- | Execute 'getRootSummary' for the 'Target's using the parallelism pipeline
-- system.
-- Create bundles of 'Target's wrapped in a 'MakeAction' that uses
-- 'withAbstractSem' to wait for a free slot, limiting the number of
-- concurrently computed summaries to the value of the @-j@ option or the slots
-- allocated by the job server, if that is used.
--
-- The 'MakeAction' returns 'Maybe', which is not handled as an error, because
-- 'runLoop' only sets it to 'Nothing' when an exception was thrown, so the
-- result won't be read anyway here.
--
-- To emulate the current behavior, we funnel exceptions past the concurrency
-- barrier and rethrow the first one afterwards.
rootSummariesParallel ::
  WorkerLimit ->
  HscEnv ->
  (GhcMessage -> AnyGhcDiagnostic) ->
  Maybe Messager ->
  (HscEnv -> Target -> IO (Either (UnitId, DriverMessages) ModSummary)) ->
  IO ([(UnitId, DriverMessages)], [ModSummary])
rootSummariesParallel n_jobs hsc_env diag_wrapper msg get_summary = do
  (actions, get_results) <- unzip <$> mapM action_and_result (zip [1..] bundles)
  runPipelines n_jobs hsc_env diag_wrapper msg actions
  (sequence . catMaybes <$> sequence get_results) >>= \case
    Right results -> pure (partitionEithers (concat results))
    Left exc -> throwIO exc
  where
    bundles = mk_bundles targets

    mk_bundles = unfoldr \case
      [] -> Nothing
      ts -> Just (splitAt bundle_size ts)

    bundle_size = 20

    targets = hsc_targets hsc_env

    action_and_result (log_queue_id, ts) = do
      res_var <- liftIO newEmptyMVar
      pure $! (MakeAction (action log_queue_id ts) res_var, readMVar res_var)

    action log_queue_id target_bundle = do
      env@MakeEnv {compile_sem} <- ask
      lift $ lift $
        withAbstractSem compile_sem $
        withLoggerHsc log_queue_id env \ lcl_hsc_env ->
          MC.try (mapM (get_summary lcl_hsc_env) target_bundle) >>= \case
            Left e | Just (_ :: SomeAsyncException) <- fromException e ->
              throwIO e
            a -> pure a

-- | This function checks then important property that if both p and q are home units
-- then any dependency of p, which transitively depends on q is also a home unit.
--
-- See Note [Multiple Home Units], section 'Closure Property'.
checkHomeUnitsClosed ::  UnitEnv -> [DriverMessages]
checkHomeUnitsClosed ue
    | Set.null bad_unit_ids = []
    | otherwise = [singleMessage $ mkPlainErrorMsgEnvelope rootLoc $ DriverHomePackagesNotClosed (Set.toList bad_unit_ids)]
  where
    home_id_set = unitEnv_keys $ ue_home_unit_graph ue
    bad_unit_ids = upwards_closure Set.\\ home_id_set
    rootLoc = mkGeneralSrcSpan (fsLit "<command line>")

    graph :: Graph (Node UnitId UnitId)
    graph = graphFromEdgedVerticesUniq graphNodes

    -- downwards closure of graph
    downwards_closure
      = graphFromEdgedVerticesUniq [ DigraphNode uid uid (Set.toList deps)
                                   | (uid, deps) <- M.toList (allReachable graph node_key)]

    inverse_closure = transposeG downwards_closure

    upwards_closure = Set.fromList $ map node_key $ reachablesG inverse_closure [DigraphNode uid uid [] | uid <- Set.toList home_id_set]

    all_unit_direct_deps :: UniqMap UnitId (Set.Set UnitId)
    all_unit_direct_deps
      = unitEnv_foldWithKey go emptyUniqMap $ ue_home_unit_graph ue
      where
        go rest this this_uis =
           plusUniqMap_C Set.union
             (addToUniqMap_C Set.union external_depends this (Set.fromList $ this_deps))
             rest
           where
             external_depends = mapUniqMap (Set.fromList . unitDepends) (unitInfoMap this_units)
             this_units = homeUnitEnv_units this_uis
             this_deps = [ toUnitId unit | (unit,Just _) <- explicitUnits this_units]

    graphNodes :: [Node UnitId UnitId]
    graphNodes = go Set.empty home_id_set
      where
        go done todo
          = case Set.minView todo of
              Nothing -> []
              Just (uid, todo')
                | Set.member uid done -> go done todo'
                | otherwise -> case lookupUniqMap all_unit_direct_deps uid of
                    Nothing -> pprPanic "uid not found" (ppr (uid, all_unit_direct_deps))
                    Just depends ->
                      let todo'' = (depends Set.\\ done) `Set.union` todo'
                      in DigraphNode uid uid (Set.toList depends) : go (Set.insert uid done) todo''

-- | Update the every ModSummary that is depended on
-- by a module that needs template haskell. We enable codegen to
-- the specified target, disable optimization and change the .hi
-- and .o file locations to be temporary files.
-- See Note [-fno-code mode]
enableCodeGenForTH
  :: Logger
  -> TmpFs
  -> UnitEnv
  -> [ModuleGraphNode]
  -> IO [ModuleGraphNode]
enableCodeGenForTH logger tmpfs unit_env =
  enableCodeGenWhen logger tmpfs TFL_CurrentModule TFL_GhcSession unit_env


data CodeGenEnable = EnableByteCode | EnableObject | EnableByteCodeAndObject deriving (Eq, Show, Ord)

instance Outputable CodeGenEnable where
  ppr = text . show

-- | Helper used to implement 'enableCodeGenForTH'.
-- In particular, this enables
-- unoptimized code generation for all modules that meet some
-- condition (first parameter), or are dependencies of those
-- modules. The second parameter is a condition to check before
-- marking modules for code generation.
enableCodeGenWhen
  :: Logger
  -> TmpFs
  -> TempFileLifetime
  -> TempFileLifetime
  -> UnitEnv
  -> [ModuleGraphNode]
  -> IO [ModuleGraphNode]
enableCodeGenWhen logger tmpfs staticLife dynLife unit_env mod_graph =
  mapM enable_code_gen mod_graph
  where
    defaultBackendOf ms = platformDefaultBackend (targetPlatform $ ue_unitFlags (ms_unitid ms) unit_env)
    enable_code_gen :: ModuleGraphNode -> IO ModuleGraphNode
    enable_code_gen n@(ModuleNode deps ms)
      | ModSummary
        { ms_location = ms_location
        , ms_hsc_src = HsSrcFile
        , ms_hspp_opts = dflags
        } <- ms
      , Just enable_spec <- mkNodeKey n `Map.lookup` needs_codegen_map =
      if | nocode_enable ms -> do
               let new_temp_file suf dynsuf = do
                     tn <- newTempName logger tmpfs (tmpDir dflags) staticLife suf
                     let dyn_tn = tn -<.> dynsuf
                     addFilesToClean tmpfs dynLife [dyn_tn]
                     return (unsafeEncodeUtf tn, unsafeEncodeUtf dyn_tn)
                 -- We don't want to create .o or .hi files unless we have been asked
                 -- to by the user. But we need them, so we patch their locations in
                 -- the ModSummary with temporary files.
                 --
               ((hi_file, dyn_hi_file), (o_file, dyn_o_file)) <-
                 -- If ``-fwrite-interface` is specified, then the .o and .hi files
                 -- are written into `-odir` and `-hidir` respectively.  #16670
                 if gopt Opt_WriteInterface dflags
                   then return ((ml_hi_file_ospath ms_location, ml_dyn_hi_file_ospath ms_location)
                               , (ml_obj_file_ospath ms_location, ml_dyn_obj_file_ospath ms_location))
                   else (,) <$> (new_temp_file (hiSuf_ dflags) (dynHiSuf_ dflags))
                            <*> (new_temp_file (objectSuf_ dflags) (dynObjectSuf_ dflags))
               let new_dflags = case enable_spec of
                                  EnableByteCode -> dflags { backend = interpreterBackend }
                                  EnableObject   -> dflags { backend = defaultBackendOf ms }
                                  EnableByteCodeAndObject -> (gopt_set dflags Opt_ByteCodeAndObjectCode) { backend = defaultBackendOf ms}
               let ms' = ms
                     { ms_location =
                         ms_location { ml_hi_file_ospath = hi_file
                                     , ml_obj_file_ospath = o_file
                                     , ml_dyn_hi_file_ospath = dyn_hi_file
                                     , ml_dyn_obj_file_ospath = dyn_o_file }
                     , ms_hspp_opts = updOptLevel 0 $ new_dflags
                     }
               -- Recursive call to catch the other cases
               enable_code_gen (ModuleNode deps ms')

         -- If -fprefer-byte-code then satisfy dependency by enabling bytecode (if normal object not enough)
         -- we only get to this case if the default backend is already generating object files, but we need dynamic
         -- objects
         | bytecode_and_enable enable_spec ms -> do
               let ms' = ms
                     { ms_hspp_opts = gopt_set (ms_hspp_opts ms) Opt_ByteCodeAndObjectCode
                     }
               -- Recursive call to catch the other cases
               enable_code_gen (ModuleNode deps ms')
         | dynamic_too_enable enable_spec ms -> do
               let ms' = ms
                     { ms_hspp_opts = gopt_set (ms_hspp_opts ms) Opt_BuildDynamicToo
                     }
               -- Recursive call to catch the other cases
               enable_code_gen (ModuleNode deps ms')
         | ext_interp_enable ms -> do
               let ms' = ms
                     { ms_hspp_opts = gopt_set (ms_hspp_opts ms) Opt_ExternalInterpreter
                     }
               -- Recursive call to catch the other cases
               enable_code_gen (ModuleNode deps ms')

         | otherwise -> return n

    enable_code_gen ms = return ms

    nocode_enable ms@(ModSummary { ms_hspp_opts = dflags }) =
      not (backendGeneratesCode (backend dflags)) &&
      -- Don't enable codegen for TH on indefinite packages; we
      -- can't compile anything anyway! See #16219.
      isHomeUnitDefinite (ue_unitHomeUnit (ms_unitid ms) unit_env)

    bytecode_and_enable enable_spec ms =
      -- In the situation where we **would** need to enable dynamic-too
      -- IF we had decided we needed objects
      dynamic_too_enable EnableObject ms
        -- but we prefer to use bytecode rather than objects
        && prefer_bytecode
        -- and we haven't already turned it on
        && not generate_both
      where
        lcl_dflags   = ms_hspp_opts ms
        prefer_bytecode = case enable_spec of
                            EnableByteCodeAndObject -> True
                            EnableByteCode -> True
                            EnableObject -> False

        generate_both   = gopt Opt_ByteCodeAndObjectCode lcl_dflags

    -- #8180 - when using TemplateHaskell, switch on -dynamic-too so
    -- the linker can correctly load the object files.  This isn't necessary
    -- when using -fexternal-interpreter.
    dynamic_too_enable enable_spec ms
      | sTargetRTSLinkerOnlySupportsSharedLibs $ settings lcl_dflags =
          not isDynWay && not dyn_too_enabled
            && enable_object
      | otherwise =
          hostIsDynamic && not hostIsProfiled && internalInterpreter &&
            not isDynWay && not isProfWay &&  not dyn_too_enabled
              && enable_object
      where
       lcl_dflags   = ms_hspp_opts ms
       internalInterpreter = not (gopt Opt_ExternalInterpreter lcl_dflags)
       dyn_too_enabled = gopt Opt_BuildDynamicToo lcl_dflags
       isDynWay    = hasWay (ways lcl_dflags) WayDyn
       isProfWay   = hasWay (ways lcl_dflags) WayProf
       enable_object = case enable_spec of
                            EnableByteCode -> False
                            EnableByteCodeAndObject -> True
                            EnableObject -> True

    -- #16331 - when no "internal interpreter" is available but we
    -- need to process some TemplateHaskell or QuasiQuotes, we automatically
    -- turn on -fexternal-interpreter.
    ext_interp_enable ms = not ghciSupported && internalInterpreter
      where
       lcl_dflags   = ms_hspp_opts ms
       internalInterpreter = not (gopt Opt_ExternalInterpreter lcl_dflags)

    (mg, lookup_node) = moduleGraphNodes False mod_graph

    mk_needed_set roots = Set.fromList $ map (mkNodeKey . node_payload) $ reachablesG mg (map (expectJust "needs_th" . lookup_node) roots)

    needs_obj_set, needs_bc_set :: Set.Set NodeKey
    needs_obj_set = mk_needed_set need_obj_set

    needs_bc_set = mk_needed_set need_bc_set

    -- A map which tells us how to enable code generation for a NodeKey
    needs_codegen_map :: Map.Map NodeKey CodeGenEnable
    needs_codegen_map =
      -- Another option here would be to just produce object code, rather than both object and
      -- byte code
      Map.unionWith (\_ _ -> EnableByteCodeAndObject)
        (Map.fromList $ [(m, EnableObject) | m <- Set.toList needs_obj_set])
        (Map.fromList $ [(m, EnableByteCode) | m <- Set.toList needs_bc_set])

    -- The direct dependencies of modules which require object code
    need_obj_set =
      concat
        -- Note we don't need object code for a module if it uses TemplateHaskell itself. Only
        -- it's dependencies.
        [ deps
        | (ModuleNode deps ms) <- mod_graph
        , isTemplateHaskellOrQQNonBoot ms
        , not (gopt Opt_UseBytecodeRatherThanObjects (ms_hspp_opts ms))
        ]

    -- The direct dependencies of modules which require byte code
    need_bc_set =
      concat
        [ deps
        | (ModuleNode deps ms) <- mod_graph
        , isTemplateHaskellOrQQNonBoot ms
        , gopt Opt_UseBytecodeRatherThanObjects (ms_hspp_opts ms)
        ]

-- | Populate the Downsweep cache with the root modules.
mkRootMap
  :: [ModSummary]
  -> DownsweepCache
mkRootMap summaries = Map.fromListWith (flip (++))
  [ ((ms_unitid s, NoPkgQual, ms_mnwib s), [Right s]) | s <- summaries ]

-----------------------------------------------------------------------------
-- Summarising modules

-- We have two types of summarisation:
--
--    * Summarise a file.  This is used for the root module(s) passed to
--      cmLoadModules.  The file is read, and used to determine the root
--      module name.  The module name may differ from the filename.
--
--    * Summarise a module.  We are given a module name, and must provide
--      a summary.  The finder is used to locate the file in which the module
--      resides.

summariseFile
        :: HscEnv
        -> HomeUnit
        -> M.Map (UnitId, FilePath) ModSummary    -- old summaries
        -> FilePath                     -- source file name
        -> Maybe Phase                  -- start phase
        -> Maybe (StringBuffer,UTCTime)
        -> IO (Either DriverMessages ModSummary)

summariseFile hsc_env' home_unit old_summaries src_fn mb_phase maybe_buf
        -- we can use a cached summary if one is available and the
        -- source file hasn't changed,
   | Just old_summary <- M.lookup (homeUnitId home_unit, src_fn) old_summaries
   = do
        let location = ms_location $ old_summary

        src_hash <- get_src_hash
                -- The file exists; we checked in getRootSummary above.
                -- If it gets removed subsequently, then this
                -- getFileHash may fail, but that's the right
                -- behaviour.

                -- return the cached summary if the source didn't change
        checkSummaryHash
            hsc_env (new_summary src_fn)
            old_summary location src_hash

   | otherwise
   = do src_hash <- get_src_hash
        new_summary src_fn src_hash
  where
    -- change the main active unit so all operations happen relative to the given unit
    hsc_env = hscSetActiveHomeUnit home_unit hsc_env'
    -- src_fn does not necessarily exist on the filesystem, so we need to
    -- check what kind of target we are dealing with
    get_src_hash = case maybe_buf of
                      Just (buf,_) -> return $ fingerprintStringBuffer buf
                      Nothing -> liftIO $ getFileHash src_fn

    new_summary src_fn src_hash = runExceptT $ do
        preimps@PreprocessedImports {..}
            <- getPreprocessedImports hsc_env src_fn mb_phase maybe_buf

        let fopts = initFinderOpts (hsc_dflags hsc_env)
            (basename, extension) = splitExtension src_fn

            hsc_src
              | isHaskellSigSuffix (drop 1 extension) = HsigFile
              | isHaskellBootSuffix (drop 1 extension) = HsBootFile
              | otherwise = HsSrcFile

            -- Make a ModLocation for this file, adding the @-boot@ suffix to
            -- all paths if the original was a boot file.
            location = mkHomeModLocation fopts pi_mod_name (unsafeEncodeUtf basename) (unsafeEncodeUtf extension) hsc_src

        -- Tell the Finder cache where it is, so that subsequent calls
        -- to findModule will find it, even if it's not on any search path
        mod <- liftIO $ do
          let home_unit = hsc_home_unit hsc_env
          let fc        = hsc_FC hsc_env
          addHomeModuleToFinder fc home_unit pi_mod_name location hsc_src

        liftIO $ makeNewModSummary hsc_env $ MakeNewModSummary
            { nms_src_fn = src_fn
            , nms_src_hash = src_hash
            , nms_hsc_src = hsc_src
            , nms_location = location
            , nms_mod = mod
            , nms_preimps = preimps
            }

checkSummaryHash
    :: HscEnv
    -> (Fingerprint -> IO (Either e ModSummary))
    -> ModSummary -> ModLocation -> Fingerprint
    -> IO (Either e ModSummary)
checkSummaryHash
  hsc_env new_summary
  old_summary
  location src_hash
  | ms_hs_hash old_summary == src_hash &&
      not (gopt Opt_ForceRecomp (hsc_dflags hsc_env)) = do
           -- update the object-file timestamp
           obj_timestamp <- modificationTimeIfExists (ml_obj_file location)

           -- We have to repopulate the Finder's cache for file targets
           -- because the file might not even be on the regular search path
           -- and it was likely flushed in depanal. This is not technically
           -- needed when we're called from sumariseModule but it shouldn't
           -- hurt.
           let fc      = hsc_FC hsc_env
               mod     = ms_mod old_summary
               hsc_src = ms_hsc_src old_summary
           addModuleToFinder fc mod location hsc_src

           hi_timestamp <- modificationTimeIfExists (ml_hi_file location)
           hie_timestamp <- modificationTimeIfExists (ml_hie_file location)

           return $ Right
             ( old_summary
                     { ms_obj_date = obj_timestamp
                     , ms_iface_date = hi_timestamp
                     , ms_hie_date = hie_timestamp
                     }
             )

   | otherwise =
           -- source changed: re-summarise.
           new_summary src_hash

data SummariseResult =
        FoundInstantiation InstantiatedUnit
      | FoundHomeWithError (UnitId, DriverMessages)
      | FoundHome ModSummary
      | External UnitId
      | NotThere

-- Summarise a module, and pick up source and timestamp.
summariseModule
          :: HscEnv
          -> HomeUnit
          -> M.Map (UnitId, FilePath) ModSummary
          -- ^ Map of old summaries
          -> IsBootInterface    -- True <=> a {-# SOURCE #-} import
          -> Located ModuleName -- Imported module to be summarised
          -> PkgQual
          -> Maybe (StringBuffer, UTCTime)
          -> [ModuleName]               -- Modules to exclude
          -> IO SummariseResult


summariseModule hsc_env' home_unit old_summary_map is_boot (L _ wanted_mod) mb_pkg
                maybe_buf excl_mods
  | wanted_mod `elem` excl_mods
  = return NotThere
  | otherwise  = find_it
  where
    -- Temporarily change the currently active home unit so all operations
    -- happen relative to it
    hsc_env   = hscSetActiveHomeUnit home_unit hsc_env'
    dflags    = hsc_dflags hsc_env

    find_it :: IO SummariseResult

    find_it = do
        found <- findImportedModuleWithIsBoot hsc_env wanted_mod is_boot mb_pkg
        case found of
             Found location mod
                | isJust (ml_hs_file location) ->
                        -- Home package
                         just_found location mod
                | VirtUnit iud <- moduleUnit mod
                , not (isHomeModule home_unit mod)
                  -> return $ FoundInstantiation iud
                | otherwise -> return $ External (moduleUnitId mod)
             _ -> return NotThere
                        -- Not found
                        -- (If it is TRULY not found at all, we'll
                        -- error when we actually try to compile)

    just_found location mod = do
                -- Adjust location to point to the hs-boot source file,
                -- hi file, object file, when is_boot says so
        let src_fn = expectJust "summarise2" (ml_hs_file location)

                -- Check that it exists
                -- It might have been deleted since the Finder last found it
        maybe_h <- fileHashIfExists src_fn
        case maybe_h of
          -- This situation can also happen if we have found the .hs file but the
          -- .hs-boot file doesn't exist.
          Nothing -> return NotThere
          Just h  -> do
            fresult <- new_summary_cache_check location mod src_fn h
            return $ case fresult of
              Left err -> FoundHomeWithError (moduleUnitId mod, err)
              Right ms -> FoundHome ms

    new_summary_cache_check loc mod src_fn h
      | Just old_summary <- Map.lookup ((toUnitId (moduleUnit mod), src_fn)) old_summary_map =

         -- check the hash on the source file, and
         -- return the cached summary if it hasn't changed.  If the
         -- file has changed then need to resummarise.
        case maybe_buf of
           Just (buf,_) ->
               checkSummaryHash hsc_env (new_summary loc mod src_fn) old_summary loc (fingerprintStringBuffer buf)
           Nothing    ->
               checkSummaryHash hsc_env (new_summary loc mod src_fn) old_summary loc h
      | otherwise = new_summary loc mod src_fn h

    new_summary :: ModLocation
                  -> Module
                  -> FilePath
                  -> Fingerprint
                  -> IO (Either DriverMessages ModSummary)
    new_summary location mod src_fn src_hash
      = runExceptT $ do
        preimps@PreprocessedImports {..}
            -- Remember to set the active unit here, otherwise the wrong include paths are passed to CPP
            -- See multiHomeUnits_cpp2 test
            <- getPreprocessedImports (hscSetActiveUnitId (moduleUnitId mod) hsc_env) src_fn Nothing maybe_buf

        -- NB: Despite the fact that is_boot is a top-level parameter, we
        -- don't actually know coming into this function what the HscSource
        -- of the module in question is.  This is because we may be processing
        -- this module because another module in the graph imported it: in this
        -- case, we know if it's a boot or not because of the {-# SOURCE #-}
        -- annotation, but we don't know if it's a signature or a regular
        -- module until we actually look it up on the filesystem.
        let hsc_src
              | is_boot == IsBoot           = HsBootFile
              | isHaskellSigFilename src_fn = HsigFile
              | otherwise                   = HsSrcFile

        when (pi_mod_name /= moduleName mod) $
                throwE $ singleMessage $ mkPlainErrorMsgEnvelope pi_mod_name_loc
                       $ DriverFileModuleNameMismatch pi_mod_name (moduleName mod)

        let instantiations = homeUnitInstantiations home_unit
        when (hsc_src == HsigFile && isNothing (lookup pi_mod_name instantiations)) $
            throwE $ singleMessage $ mkPlainErrorMsgEnvelope pi_mod_name_loc
                   $ DriverUnexpectedSignature pi_mod_name (checkBuildingCabalPackage dflags) instantiations

        liftIO $ makeNewModSummary hsc_env $ MakeNewModSummary
            { nms_src_fn = src_fn
            , nms_src_hash = src_hash
            , nms_hsc_src = hsc_src
            , nms_location = location
            , nms_mod = mod
            , nms_preimps = preimps
            }

-- | Convenience named arguments for 'makeNewModSummary' only used to make
-- code more readable, not exported.
data MakeNewModSummary
  = MakeNewModSummary
      { nms_src_fn :: FilePath
      , nms_src_hash :: Fingerprint
      , nms_hsc_src :: HscSource
      , nms_location :: ModLocation
      , nms_mod :: Module
      , nms_preimps :: PreprocessedImports
      }

makeNewModSummary :: HscEnv -> MakeNewModSummary -> IO ModSummary
makeNewModSummary hsc_env MakeNewModSummary{..} = do
  let PreprocessedImports{..} = nms_preimps
  obj_timestamp <- modificationTimeIfExists (ml_obj_file nms_location)
  dyn_obj_timestamp <- modificationTimeIfExists (ml_dyn_obj_file nms_location)
  hi_timestamp <- modificationTimeIfExists (ml_hi_file nms_location)
  hie_timestamp <- modificationTimeIfExists (ml_hie_file nms_location)

  extra_sig_imports <- findExtraSigImports hsc_env nms_hsc_src pi_mod_name
  (implicit_sigs, _inst_deps) <- implicitRequirementsShallow (hscSetActiveUnitId (moduleUnitId nms_mod) hsc_env) pi_theimps

  return $
        ModSummary
        { ms_mod = nms_mod
        , ms_hsc_src = nms_hsc_src
        , ms_location = nms_location
        , ms_hspp_file = pi_hspp_fn
        , ms_hspp_opts = pi_local_dflags
        , ms_hspp_buf  = Just pi_hspp_buf
        , ms_parsed_mod = Nothing
        , ms_srcimps = pi_srcimps
        , ms_ghc_prim_import = pi_ghc_prim_import
        , ms_textual_imps =
            ((,) NoPkgQual . noLoc <$> extra_sig_imports) ++
            ((,) NoPkgQual . noLoc <$> implicit_sigs) ++
            pi_theimps
        , ms_hs_hash = nms_src_hash
        , ms_iface_date = hi_timestamp
        , ms_hie_date = hie_timestamp
        , ms_obj_date = obj_timestamp
        , ms_dyn_obj_date = dyn_obj_timestamp
        }

data PreprocessedImports
  = PreprocessedImports
      { pi_local_dflags :: DynFlags
      , pi_srcimps  :: [(PkgQual, Located ModuleName)]
      , pi_theimps  :: [(PkgQual, Located ModuleName)]
      , pi_ghc_prim_import :: Bool
      , pi_hspp_fn  :: FilePath
      , pi_hspp_buf :: StringBuffer
      , pi_mod_name_loc :: SrcSpan
      , pi_mod_name :: ModuleName
      }

-- Preprocess the source file and get its imports
-- The pi_local_dflags contains the OPTIONS pragmas
getPreprocessedImports
    :: HscEnv
    -> FilePath
    -> Maybe Phase
    -> Maybe (StringBuffer, UTCTime)
    -- ^ optional source code buffer and modification time
    -> ExceptT DriverMessages IO PreprocessedImports
getPreprocessedImports hsc_env src_fn mb_phase maybe_buf = do
  (pi_local_dflags, pi_hspp_fn)
      <- ExceptT $ preprocess hsc_env src_fn (fst <$> maybe_buf) mb_phase
  pi_hspp_buf <- liftIO $ hGetStringBuffer pi_hspp_fn
  (pi_srcimps', pi_theimps', pi_ghc_prim_import, L pi_mod_name_loc pi_mod_name)
      <- ExceptT $ do
          let imp_prelude = xopt LangExt.ImplicitPrelude pi_local_dflags
              popts = initParserOpts pi_local_dflags
          mimps <- getImports popts imp_prelude pi_hspp_buf pi_hspp_fn src_fn
          return (first (mkMessages . fmap mkDriverPsHeaderMessage . getMessages) mimps)
  let rn_pkg_qual = renameRawPkgQual (hsc_unit_env hsc_env)
  let rn_imps = fmap (\(pk, lmn@(L _ mn)) -> (rn_pkg_qual mn pk, lmn))
  let pi_srcimps = rn_imps pi_srcimps'
  let pi_theimps = rn_imps pi_theimps'
  return PreprocessedImports {..}


-----------------------------------------------------------------------------
--                      Error messages
-----------------------------------------------------------------------------

-- Defer and group warning, error and fatal messages so they will not get lost
-- in the regular output.
withDeferredDiagnostics :: GhcMonad m => m a -> m a
withDeferredDiagnostics f = do
  dflags <- getDynFlags
  if not $ gopt Opt_DeferDiagnostics dflags
  then f
  else do
    warnings <- liftIO $ newIORef []
    errors <- liftIO $ newIORef []
    fatals <- liftIO $ newIORef []
    logger <- getLogger

    let deferDiagnostics _dflags !msgClass !srcSpan !msg = do
          let action = logMsg logger msgClass srcSpan msg
          case msgClass of
            MCDiagnostic SevWarning _reason _code
              -> atomicModifyIORef' warnings $ \(!i) -> (action: i, ())
            MCDiagnostic SevError _reason _code
              -> atomicModifyIORef' errors   $ \(!i) -> (action: i, ())
            MCFatal
              -> atomicModifyIORef' fatals   $ \(!i) -> (action: i, ())
            _ -> action

        printDeferredDiagnostics = liftIO $
          forM_ [warnings, errors, fatals] $ \ref -> do
            -- This IORef can leak when the dflags leaks, so let us always
            -- reset the content. The lazy variant is used here as we want to force
            -- this error if the IORef is ever accessed again, rather than now.
            -- See #20981 for an issue which discusses this general issue.
            let landmine = if debugIsOn then panic "withDeferredDiagnostics: use after free" else []
            actions <- atomicModifyIORef ref $ \i -> (landmine, i)
            sequence_ $ reverse actions

    MC.bracket
      (pushLogHookM (const deferDiagnostics))
      (\_ -> popLogHookM >> printDeferredDiagnostics)
      (\_ -> f)

noModError :: HscEnv -> SrcSpan -> ModuleName -> FindResult -> MsgEnvelope GhcMessage
-- ToDo: we don't have a proper line number for this error
noModError hsc_env loc wanted_mod err
  = mkPlainErrorMsgEnvelope loc $ GhcDriverMessage $
    DriverInterfaceError $
    (Can'tFindInterface (cannotFindModule hsc_env wanted_mod err) (LookingForModule wanted_mod NotBoot))

{-
noHsFileErr :: SrcSpan -> String -> DriverMessages
noHsFileErr loc path
  = singleMessage $ mkPlainErrorMsgEnvelope loc (DriverFileNotFound path)
  -}

moduleNotFoundErr :: ModuleName -> DriverMessages
moduleNotFoundErr mod = singleMessage $ mkPlainErrorMsgEnvelope noSrcSpan (DriverModuleNotFound mod)

multiRootsErr :: [ModSummary] -> IO ()
multiRootsErr [] = panic "multiRootsErr"
multiRootsErr summs@(summ1:_)
  = throwOneError $ fmap GhcDriverMessage $
    mkPlainErrorMsgEnvelope noSrcSpan $ DriverDuplicatedModuleDeclaration mod files
  where
    mod = ms_mod summ1
    files = map (expectJust "checkDup" . ml_hs_file . ms_location) summs

cyclicModuleErr :: [ModuleGraphNode] -> MsgEnvelope GhcMessage
-- From a strongly connected component we find
-- a single cycle to report
cyclicModuleErr mss
  = assert (not (null mss)) $
    case findCycle graph of
       Nothing   -> pprPanic "Unexpected non-cycle" (ppr mss)
       Just path -> mkPlainErrorMsgEnvelope src_span $
                    GhcDriverMessage $ DriverModuleGraphCycle path
        where
          src_span = maybe noSrcSpan (mkFileSrcSpan . ms_location) (moduleGraphNodeModSum (head path))
  where
    graph :: [Node NodeKey ModuleGraphNode]
    graph =
      [ DigraphNode
        { node_payload = ms
        , node_key = mkNodeKey ms
        , node_dependencies = nodeDependencies False ms
        }
      | ms <- mss
      ]

cleanCurrentModuleTempFilesMaybe :: MonadIO m => Logger -> TmpFs -> DynFlags -> m ()
cleanCurrentModuleTempFilesMaybe logger tmpfs dflags =
  if gopt Opt_KeepTmpFiles dflags
    then liftIO $ keepCurrentModuleTempFiles logger tmpfs
    else liftIO $ cleanCurrentModuleTempFiles logger tmpfs


addDepsToHscEnv ::  [HomeModInfo] -> HscEnv -> HscEnv
addDepsToHscEnv deps hsc_env =
  hscUpdateHUG (\hug -> foldr addHomeModInfoToHug hug deps) hsc_env

setHPT ::  HomePackageTable -> HscEnv -> HscEnv
setHPT deps hsc_env =
  hscUpdateHPT (const $ deps) hsc_env

setHUG ::  HomeUnitGraph -> HscEnv -> HscEnv
setHUG deps hsc_env =
  hscUpdateHUG (const $ deps) hsc_env

-- | Wrap an action to catch and handle exceptions.
wrapAction :: (GhcMessage -> AnyGhcDiagnostic) -> HscEnv -> IO a -> IO (Maybe a)
wrapAction msg_wrapper hsc_env k = do
  let lcl_logger = hsc_logger hsc_env
      lcl_dynflags = hsc_dflags hsc_env
      print_config = initPrintConfig lcl_dynflags
      logg err = printMessages lcl_logger print_config (initDiagOpts lcl_dynflags) (msg_wrapper <$> srcErrorMessages err)
  -- MP: It is a bit strange how prettyPrintGhcErrors handles some errors but then we handle
  -- SourceError and ThreadKilled differently directly below. TODO: Refactor to use `catches`
  -- directly. MP should probably use safeTry here to not catch async exceptions but that will regress performance due to
  -- internally using forkIO.
  mres <- MC.try $ prettyPrintGhcErrors lcl_logger $ k
  case mres of
    Right res -> return $ Just res
    Left exc -> do
        case fromException exc of
          Just (err :: SourceError)
            -> logg err
          Nothing -> case fromException exc of
                        -- ThreadKilled in particular needs to actually kill the thread.
                        -- So rethrow that and the other async exceptions
                        Just (err :: SomeAsyncException) -> throwIO err
                        _ -> errorMsg lcl_logger (text (show exc))
        return Nothing

withParLog :: TVar LogQueueQueue -> Int -> ((Logger -> Logger) -> IO b) -> IO b
withParLog lqq_var k cont = do
  let init_log = do
        -- Make a new log queue
        lq <- newLogQueue k
        -- Add it into the LogQueueQueue
        atomically $ initLogQueue lqq_var lq
        return lq
      finish_log lq = liftIO (finishLogQueue lq)
  MC.bracket init_log finish_log $ \lq -> cont (pushLogHook (const (parLogAction lq)))

withLoggerHsc :: Int -> MakeEnv -> (HscEnv -> IO a) -> IO a
withLoggerHsc k MakeEnv{withLogger, hsc_env} cont = do
  withLogger k $ \modifyLogger -> do
    let lcl_logger = modifyLogger (hsc_logger hsc_env)
        hsc_env' = hsc_env { hsc_logger = lcl_logger }
    -- Run continuation with modified logger
    cont hsc_env'


executeInstantiationNode :: Int
  -> Int
  -> HomeUnitGraph
  -> UnitId
  -> InstantiatedUnit
  -> RunMakeM ()
executeInstantiationNode k n deps uid iu = do
        env <- ask
        -- Output of the logger is mediated by a central worker to
        -- avoid output interleaving
        msg <- asks env_messager
        wrapper <- asks diag_wrapper
        lift $ MaybeT $ withLoggerHsc k env $ \hsc_env ->
          let lcl_hsc_env = setHUG deps hsc_env
          in wrapAction wrapper lcl_hsc_env $ do
            res <- upsweep_inst lcl_hsc_env msg k n uid iu
            cleanCurrentModuleTempFilesMaybe (hsc_logger hsc_env) (hsc_tmpfs hsc_env) (hsc_dflags hsc_env)
            return res


executeCompileNode :: Int
  -> Int
  -> Maybe HomeModInfo
  -> HomeUnitGraph
  -> Maybe [ModuleName] -- List of modules we need to rehydrate before compiling
  -> ModSummary
  -> RunMakeM HomeModInfo
executeCompileNode k n !old_hmi hug mrehydrate_mods mod = do
  me@MakeEnv{..} <- ask
  -- Rehydrate any dependencies if this module had a boot file or is a signature file.
  lift $ MaybeT (withAbstractSem compile_sem $ withLoggerHsc k me $ \hsc_env -> do
     hydrated_hsc_env <- liftIO $ maybeRehydrateBefore (setHUG hug hsc_env) mod fixed_mrehydrate_mods
     let -- Use the cached DynFlags which includes OPTIONS_GHC pragmas
         lcl_dynflags = ms_hspp_opts mod
     let lcl_hsc_env =
             -- Localise the hsc_env to use the cached flags
             hscSetFlags lcl_dynflags $
             hydrated_hsc_env
     -- Compile the module, locking with a semaphore to avoid too many modules
     -- being compiled at the same time leading to high memory usage.
     wrapAction diag_wrapper lcl_hsc_env $ do
      res <- upsweep_mod lcl_hsc_env env_messager old_hmi mod k n
      cleanCurrentModuleTempFilesMaybe (hsc_logger hsc_env) (hsc_tmpfs hsc_env) lcl_dynflags
      return res)

  where
    fixed_mrehydrate_mods =
      case ms_hsc_src mod of
        -- MP: It is probably a bit of a misimplementation in backpack that
        -- compiling a signature requires an knot_var for that unit.
        -- If you remove this then a lot of backpack tests fail.
        HsigFile -> Just []
        _        -> mrehydrate_mods

{- Rehydration, see Note [Rehydrating Modules] -}

rehydrate :: HscEnv        -- ^ The HPT in this HscEnv needs rehydrating.
          -> [HomeModInfo] -- ^ These are the modules we want to rehydrate.
          -> IO HscEnv
rehydrate hsc_env hmis = do
  debugTraceMsg logger 2 $ (
     text "Re-hydrating loop: " <+> (ppr (map (mi_module . hm_iface) hmis)))
  new_mods <- fixIO $ \new_mods -> do
      let new_hpt = addListToHpt old_hpt new_mods
      let new_hsc_env = hscUpdateHPT_lazy (const new_hpt) hsc_env
      mds <- initIfaceCheck (text "rehydrate") new_hsc_env $
                mapM (typecheckIface . hm_iface) hmis
      let new_mods = [ (mn,hmi{ hm_details = details })
                     | (hmi,details) <- zip hmis mds
                     , let mn = moduleName (mi_module (hm_iface hmi)) ]
      return new_mods
  return $ setHPT (foldl' (\old (mn, hmi) -> addToHpt old mn hmi) old_hpt new_mods) hsc_env

  where
    logger  = hsc_logger hsc_env
    to_delete =  (map (moduleName . mi_module . hm_iface) hmis)
    -- Filter out old modules before tying the knot, otherwise we can end
    -- up with a thunk which keeps reference to the old HomeModInfo.
    !old_hpt = foldl' delFromHpt (hsc_HPT hsc_env) to_delete

-- If needed, then rehydrate the necessary modules with a suitable KnotVars for the
-- module currently being compiled.
maybeRehydrateBefore :: HscEnv -> ModSummary -> Maybe [ModuleName] -> IO HscEnv
maybeRehydrateBefore hsc_env _ Nothing = return hsc_env
maybeRehydrateBefore hsc_env mod (Just mns) = do
  knot_var <- initialise_knot_var hsc_env
  let hmis = map (expectJust "mr" . lookupHpt (hsc_HPT hsc_env)) mns
  rehydrate (hsc_env { hsc_type_env_vars = knotVarsFromModuleEnv knot_var }) hmis

  where
   initialise_knot_var hsc_env = liftIO $
    let mod_name = homeModuleInstantiation (hsc_home_unit_maybe hsc_env) (ms_mod mod)
    in mkModuleEnv . (:[]) . (mod_name,) <$> newIORef emptyTypeEnv

rehydrateAfter :: HscEnv
  -> [ModuleName]
  -> IO [HomeModInfo]
rehydrateAfter new_hsc mns = do
  let new_hpt = hsc_HPT new_hsc
      hmis = map (expectJust "mrAfter" . lookupHpt new_hpt) mns
  hsc_env <- rehydrate (new_hsc { hsc_type_env_vars = emptyKnotVars }) hmis
  return $ map (\mn -> expectJust "rehydrate" $ lookupHpt (hsc_HPT hsc_env) mn) mns

{-
Note [Hydrating Modules]
~~~~~~~~~~~~~~~~~~~~~~~~
There are at least 4 different representations of an interface file as described
by this diagram.

------------------------------
|       On-disk M.hi         |
------------------------------
    |             ^
    | Read file   | Write file
    V             |
-------------------------------
|      ByteString             |
-------------------------------
    |             ^
    | Binary.get  | Binary.put
    V             |
--------------------------------
|    ModIface (an acyclic AST) |
--------------------------------
    |           ^
    | hydrate   | mkIfaceTc
    V           |
---------------------------------
|  ModDetails (lots of cycles)  |
---------------------------------

The last step, converting a ModIface into a ModDetails is known as "hydration".

Hydration happens in three different places

* When an interface file is initially loaded from disk, it has to be hydrated.
* When a module is finished compiling, we hydrate the ModIface in order to generate
  the version of ModDetails which exists in memory (see Note [ModDetails and --make mode])
* When dealing with boot files and module loops (see Note [Rehydrating Modules])

Note [Rehydrating Modules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
If a module has a boot file then it is critical to rehydrate the modules on
the path between the two (see #20561).

Suppose we have ("R" for "recursive"):
```
R.hs-boot:   module R where
               data T
               g :: T -> T

A.hs:        module A( f, T, g ) where
                import {-# SOURCE #-} R
                data S = MkS T
                f :: T -> S = ...g...

R.hs:        module R where
                import A
                data T = T1 | T2 S
                g = ...f...
```

== Why we need to rehydrate A's ModIface before compiling R.hs

After compiling A.hs we'll have a TypeEnv in which the Id for `f` has a type
that uses the AbstractTyCon T; and a TyCon for `S` that also mentions that same
AbstractTyCon. (Abstract because it came from R.hs-boot; we know nothing about
it.)

When compiling R.hs, we build a TyCon for `T`.  But that TyCon mentions `S`, and
it currently has an AbstractTyCon for `T` inside it.  But we want to build a
fully cyclic structure, in which `S` refers to `T` and `T` refers to `S`.

Solution: **rehydration**.  *Before compiling `R.hs`*, rehydrate all the
ModIfaces below it that depend on R.hs-boot.  To rehydrate a ModIface, call
`typecheckIface` to convert it to a ModDetails.  It's just a de-serialisation
step, no type inference, just lookups.

Now `S` will be bound to a thunk that, when forced, will "see" the final binding
for `T`; see [Tying the knot](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/tying-the-knot).
But note that this must be done *before* compiling R.hs.

== Why we need to rehydrate A's ModIface after compiling R.hs

When compiling R.hs, the knot-tying stuff above will ensure that `f`'s unfolding
mentions the `LocalId` for `g`.  But when we finish R, we carefully ensure that
all those `LocalIds` are turned into completed `GlobalIds`, replete with
unfoldings etc.   Alas, that will not apply to the occurrences of `g` in `f`'s
unfolding. And if we leave matters like that, they will stay that way, and *all*
subsequent modules that import A will see a crippled unfolding for `f`.

Solution: rehydrate both R and A's ModIface together, right after completing R.hs.

~~ Which modules to rehydrate

We only need rehydrate modules that are
* Below R.hs
* Above R.hs-boot

There might be many unrelated modules (in the home package) that don't need to be
rehydrated.

== Loops with multiple boot files

It is possible for a module graph to have a loop (SCC, when ignoring boot files)
which requires multiple boot files to break. In this case, we must perform
several hydration steps:
  1. The hydration steps described above, which are necessary for correctness.
  2. An extra hydration step at the end of compiling the entire SCC, in order to
     remove space leaks, as we explain below.

Consider the following example:

   ┌─────┐     ┌─────┐
   │  A  │     │  B  │
   └──┬──┘     └──┬──┘
      │           │
  ┌───▼───────────▼───┐
  │         C         │
  └───┬───────────┬───┘
      │           │
 ┌────▼───┐   ┌───▼────┐
 │ A-boot │   │ B-boot │
 └────────┘   └────────┘

A, B and C live together in a SCC. Suppose that we compile the modules in the
order:

  A-boot, B-boot, C, A, B.

When we come to compile A, we will perform the necessary hydration steps,
because A has a boot file. This means that C will be hydrated relative to A,
and the ModDetails for A will reference C/A. Then, when B is compiled,
C will be rehydrated again, and so B will reference C/A,B. At this point,
its interface will be hydrated relative to both A and B.
This causes a space leak: there are now two different copies of C's ModDetails,
kept alive by modules A and B. This is especially problematic if C is large.

The way to avoid this space leak is to rehydrate an entire SCC together at the
end of compilation, so that all the ModDetails point to interfaces for .hs files.
In this example, when we hydrate A, B and C together, then both A and B will refer to
C/A,B.

See #21900 for some more discussion.

== Modules "above" the loop

This dark corner is the subject of #14092.

Suppose we add to our example
```
X.hs     module X where
           import A
           data XT = MkX T
           fx = ...g...
```
If in `--make` we compile R.hs-boot, then A.hs, then X.hs, we'll get a `ModDetails` for `X` that has an AbstractTyCon for `T` in the argument type of `MkX`.  So:

* Either we should delay compiling X until after R has been compiled. (This is what we do)
* Or we should rehydrate X after compiling R -- because it transitively depends on R.hs-boot.

Ticket #20200 has exposed some issues to do with the knot-tying logic in GHC.Make, in `--make` mode.
#20200 has lots of issues, many of them now fixed;
this particular issue starts [here](https://gitlab.haskell.org/ghc/ghc/-/issues/20200#note_385758).

The wiki page [Tying the knot](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/tying-the-knot) is helpful.
Also closely related are
    * #14092
    * #14103

-}

executeLinkNode :: HomeUnitGraph -> (Int, Int) -> UnitId -> [NodeKey] -> RunMakeM ()
executeLinkNode hug kn uid deps = do
  withCurrentUnit uid $ do
    MakeEnv{..} <- ask
    let dflags = hsc_dflags hsc_env
    let hsc_env' = setHUG hug hsc_env
        msg' = (\messager -> \recomp -> messager hsc_env kn recomp (LinkNode deps uid)) <$> env_messager

    linkresult <- liftIO $ withAbstractSem compile_sem $ do
                            link (ghcLink dflags)
                                (hsc_logger hsc_env')
                                (hsc_tmpfs hsc_env')
                                (hsc_FC hsc_env')
                                (hsc_hooks hsc_env')
                                dflags
                                (hsc_unit_env hsc_env')
                                True -- We already decided to link
                                msg'
                                (hsc_HPT hsc_env')
    case linkresult of
      Failed -> fail "Link Failed"
      Succeeded -> return ()

{-
Note [ModuleNameSet, efficiency and space leaks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

During upsweep, the results of compiling modules are placed into a MVar. When we need
to compute the right compilation environment for a module, we consult this MVar and
set the HomeUnitGraph accordingly. This is done to avoid having to precisely track
module dependencies and recreating the HUG from scratch each time, which is very expensive.

In serial mode (-j1), this all works out fine: a module can only be compiled
after its dependencies have finished compiling, and compilation can't be
interleaved with the compilation of other module loops. This ensures that
the HUG only ever contains finalised interfaces.

In parallel mode, we have to be more careful: the HUG variable can contain non-finalised
interfaces, which have been started by another thread. In order to avoid a space leak
in which a finalised interface is compiled against a HPT which contains a non-finalised
interface, we have to restrict the HUG to only contain the visible modules.

The collection of visible modules explains which transitive modules are visible
from a certain point. It is recorded in the ModuleNameSet.
Before a module is compiled, we use this set to restrict the HUG to the visible
modules only, avoiding this tricky space leak.

Efficiency of the ModuleNameSet is of utmost importance, because a union occurs for
each edge in the module graph. To achieve this, the set is represented directly as an IntSet,
which provides suitable performance – even using a UniqSet (which is backed by an IntMap) is
too slow. The crucial test of performance here is the time taken to a do a no-op build in --make mode.

See test "jspace" for an example which used to trigger this problem.

-}

-- See Note [ModuleNameSet, efficiency and space leaks]
type ModuleNameSet = M.Map UnitId W.Word64Set

addToModuleNameSet :: UnitId -> ModuleName -> ModuleNameSet -> ModuleNameSet
addToModuleNameSet uid mn s =
  let k = (getKey $ getUnique $ mn)
  in M.insertWith (W.union) uid (W.singleton k) s

-- | Wait for some dependencies to finish and then read from the given MVar.
wait_deps_hug :: MVar HomeUnitGraph -> [BuildResult] -> ReaderT MakeEnv (MaybeT IO) (HomeUnitGraph, ModuleNameSet)
wait_deps_hug hug_var deps = do
  (_, module_deps) <- wait_deps deps
  hug <- liftIO $ readMVar hug_var
  let pruneHomeUnitEnv uid hme =
        let -- Restrict to things which are in the transitive closure to avoid retaining
            -- reference to loop modules which have already been compiled by other threads.
            -- See Note [ModuleNameSet, efficiency and space leaks]
            !new = udfmRestrictKeysSet (homeUnitEnv_hpt hme) (fromMaybe W.empty $ M.lookup  uid module_deps)
        in hme { homeUnitEnv_hpt = new }
  return (unitEnv_mapWithKey pruneHomeUnitEnv hug, module_deps)

-- | Wait for dependencies to finish, and then return their results.
wait_deps :: [BuildResult] -> RunMakeM ([HomeModInfo], ModuleNameSet)
wait_deps [] = return ([], M.empty)
wait_deps (x:xs) = do
  (res, deps) <- lift $ waitResult (resultVar x)
  (hmis, all_deps) <- wait_deps xs
  let !new_deps = deps `unionModuleNameSet` all_deps
  case res of
    Nothing -> return (hmis, new_deps)
    Just hmi -> return (hmi:hmis, new_deps)
  where
    unionModuleNameSet = M.unionWith W.union


-- Executing the pipelines


label_self :: String -> IO ()
label_self thread_name = do
    self_tid <- CC.myThreadId
    CC.labelThread self_tid thread_name


runPipelines :: WorkerLimit -> HscEnv -> (GhcMessage -> AnyGhcDiagnostic) -> Maybe Messager -> [MakeAction] -> IO ()
-- Don't even initialise plugins if there are no pipelines
runPipelines n_job hsc_env diag_wrapper mHscMessager all_pipelines = do
  liftIO $ label_self "main --make thread"
  case n_job of
    NumProcessorsLimit n | n <= 1 -> runSeqPipelines hsc_env diag_wrapper mHscMessager all_pipelines
    _n -> runParPipelines n_job hsc_env diag_wrapper mHscMessager all_pipelines

runSeqPipelines :: HscEnv -> (GhcMessage -> AnyGhcDiagnostic) -> Maybe Messager -> [MakeAction] -> IO ()
runSeqPipelines plugin_hsc_env diag_wrapper mHscMessager all_pipelines =
  let env = MakeEnv { hsc_env = plugin_hsc_env
                    , withLogger = \_ k -> k id
                    , compile_sem = AbstractSem (return ()) (return ())
                    , env_messager = mHscMessager
                    , diag_wrapper = diag_wrapper
                    }
  in runAllPipelines (NumProcessorsLimit 1) env all_pipelines

runNjobsAbstractSem :: Int -> (AbstractSem -> IO a) -> IO a
runNjobsAbstractSem n_jobs action = do
  compile_sem <- newQSem n_jobs
  n_capabilities <- getNumCapabilities
  n_cpus <- getNumProcessors
  let
    asem = AbstractSem (waitQSem compile_sem) (signalQSem compile_sem)
    set_num_caps n = unless (n_capabilities /= 1) $ setNumCapabilities n
    updNumCapabilities =  do
      -- Setting number of capabilities more than
      -- CPU count usually leads to high userspace
      -- lock contention. #9221
      set_num_caps $ min n_jobs n_cpus
    resetNumCapabilities = set_num_caps n_capabilities
  MC.bracket_ updNumCapabilities resetNumCapabilities $ action asem

runWorkerLimit :: WorkerLimit -> (AbstractSem -> IO a) -> IO a
#if defined(wasm32_HOST_ARCH)
runWorkerLimit _ action = do
  lock <- newMVar ()
  action $ AbstractSem (takeMVar lock) (putMVar lock ())
#else
runWorkerLimit worker_limit action = case worker_limit of
    NumProcessorsLimit n_jobs ->
      runNjobsAbstractSem n_jobs action
    JSemLimit sem ->
      runJSemAbstractSem sem action
#endif

-- | Build and run a pipeline
runParPipelines :: WorkerLimit -- ^ How to limit work parallelism
             -> HscEnv         -- ^ The basic HscEnv which is augmented with specific info for each module
             -> (GhcMessage -> AnyGhcDiagnostic)
             -> Maybe Messager   -- ^ Optional custom messager to use to report progress
             -> [MakeAction]  -- ^ The build plan for all the module nodes
             -> IO ()
runParPipelines worker_limit plugin_hsc_env diag_wrapper mHscMessager all_pipelines = do


  -- A variable which we write to when an error has happened and we have to tell the
  -- logging thread to gracefully shut down.
  stopped_var <- newTVarIO False
  -- The queue of LogQueues which actions are able to write to. When an action starts it
  -- will add it's LogQueue into this queue.
  log_queue_queue_var <- newTVarIO newLogQueueQueue
  -- Thread which coordinates the printing of logs
  wait_log_thread <- logThread (hsc_logger plugin_hsc_env) stopped_var log_queue_queue_var


  -- Make the logger thread-safe, in case there is some output which isn't sent via the LogQueue.
  thread_safe_logger <- liftIO $ makeThreadSafe (hsc_logger plugin_hsc_env)
  let thread_safe_hsc_env = plugin_hsc_env { hsc_logger = thread_safe_logger }

  runWorkerLimit worker_limit $ \abstract_sem -> do
    let env = MakeEnv { hsc_env = thread_safe_hsc_env
                      , withLogger = withParLog log_queue_queue_var
                      , compile_sem = abstract_sem
                      , env_messager = mHscMessager
                      , diag_wrapper = diag_wrapper
                      }
    -- Reset the number of capabilities once the upsweep ends.
    runAllPipelines worker_limit env all_pipelines
    atomically $ writeTVar stopped_var True
    wait_log_thread

withLocalTmpFS :: TmpFs -> (TmpFs -> IO a) -> IO a
withLocalTmpFS tmpfs act = do
  let initialiser = do
        liftIO $ forkTmpFsFrom tmpfs
      finaliser tmpfs_local = do
        liftIO $ mergeTmpFsInto tmpfs_local tmpfs
       -- Add remaining files which weren't cleaned up into local tmp fs for
       -- clean-up later.
       -- Clear the logQueue if this node had it's own log queue
  MC.bracket initialiser finaliser act

withLocalTmpFSMake :: MakeEnv -> (MakeEnv -> IO a) -> IO a
withLocalTmpFSMake env k =
  withLocalTmpFS (hsc_tmpfs (hsc_env env)) $ \lcl_tmpfs
    -> k (env { hsc_env = (hsc_env env) { hsc_tmpfs = lcl_tmpfs }})


-- | Run the given actions and then wait for them all to finish.
runAllPipelines :: WorkerLimit -> MakeEnv -> [MakeAction] -> IO ()
runAllPipelines worker_limit env acts = do
  let single_worker = isWorkerLimitSequential worker_limit
      spawn_actions :: IO [ThreadId]
      spawn_actions = if single_worker
        then (:[]) <$> (forkIOWithUnmask $ \unmask -> void $ runLoop (\io -> io unmask) env acts)
        else runLoop forkIOWithUnmask env acts

      kill_actions :: [ThreadId] -> IO ()
      kill_actions tids = mapM_ killThread tids

  MC.bracket spawn_actions kill_actions $ \_ -> do
    mapM_ waitMakeAction acts

-- | Execute each action in order, limiting the amount of parallelism by the given
-- semaphore.
runLoop :: (((forall a. IO a -> IO a) -> IO ()) -> IO a) -> MakeEnv -> [MakeAction] -> IO [a]
runLoop _ _env [] = return []
runLoop fork_thread env (MakeAction act res_var :acts) = do

  -- withLocalTmpFs has to occur outside of fork to remain deterministic
  new_thread <- withLocalTmpFSMake env $ \lcl_env ->
    fork_thread $ \unmask -> (do
            mres <- (unmask $ run_pipeline lcl_env act)
                      `MC.onException` (putMVar res_var Nothing) -- Defensive: If there's an unhandled exception then still signal the failure.
            putMVar res_var mres)
  threads <- runLoop fork_thread env acts
  return (new_thread : threads)
  where
      run_pipeline :: MakeEnv -> RunMakeM a -> IO (Maybe a)
      run_pipeline env p = runMaybeT (runReaderT p env)

data MakeAction = forall a . MakeAction !(RunMakeM a) !(MVar (Maybe a))

waitMakeAction :: MakeAction -> IO ()
waitMakeAction (MakeAction _ mvar) = () <$ readMVar mvar

{- Note [GHC Heap Invariants]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~
This note is a general place to explain some of the heap invariants which should
hold for a program compiled with --make mode. These invariants are all things
which can be checked easily using ghc-debug.

1. No HomeModInfo are reachable via the EPS.
   Why? Interfaces are lazily loaded into the EPS and the lazy thunk retains
        a reference to the entire HscEnv, if we are not careful the HscEnv will
        contain the HomePackageTable at the time the interface was loaded and
        it will never be released.
   Where? dontLeakTheHUG in GHC.Iface.Load

2. No KnotVars are live at the end of upsweep (#20491)
   Why? KnotVars contains an old stale reference to the TypeEnv for modules
        which participate in a loop. At the end of a loop all the KnotVars references
        should be removed by the call to typecheckLoop.
   Where? typecheckLoop in GHC.Driver.Make.

3. Immediately after a reload, no ModDetails are live.
   Why? During the upsweep all old ModDetails are replaced with a new ModDetails
        generated from a ModIface. If we don't clear the ModDetails before the
        reload takes place then memory usage during the reload is twice as much
        as it should be as we retain a copy of the ModDetails for too long.
   Where? pruneCache in GHC.Driver.Make

4. No TcGblEnv or TcLclEnv are live after typechecking is completed.
   Why? By the time we get to simplification all the data structures from typechecking
        should be eliminated.
   Where? No one place in the compiler. These leaks can be introduced by not suitable
          forcing functions which take a TcLclEnv as an argument.

5. At the end of a successful upsweep, the number of live ModDetails equals the
   number of non-boot Modules.
   Why? Each module has a HomeModInfo which contains a ModDetails from that module.
   Where? See Note [ModuleNameSet, efficiency and space leaks], a variety of places
          in the driver are responsible.
-}
