{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2011
--
-- This module implements multi-module compilation, and is used
-- by --make and GHCi.
--
-- -----------------------------------------------------------------------------
module GHC.Driver.Make (
        depanal, depanalE, depanalPartial,
        load, loadWithCache, load', AnyGhcDiagnostic, LoadHowMuch(..), ModIfaceCache(..), noIfaceCache, newIfaceCache,

        downsweep,

        topSortModuleGraph,

        ms_home_srcimps, ms_home_imps,

        hscSourceToIsBoot,
        findExtraSigImports,
        implicitRequirementsShallow,

        noModError, cyclicModuleErr,
        SummaryNode,
        IsBootInterface(..), mkNodeKey,

        ModNodeKey, ModNodeKeyWithUid(..),
        ModNodeMap(..), emptyModNodeMap, modNodeMapElems, modNodeMapLookup, modNodeMapInsert, modNodeMapSingleton, modNodeMapUnionWith,

        -- * Re-exports from Downsweep
        checkHomeUnitsClosed,
        summariseModule,
        summariseModuleInterface,
        SummariseResult(..),
        summariseFile,

        instantiationNodes,
        ) where

import GHC.Prelude
import GHC.Platform

import GHC.Tc.Utils.Backpack
import GHC.Tc.Utils.Monad  ( initIfaceCheck, concatMapM )

import GHC.Runtime.Interpreter
import qualified GHC.Linker.Loader as Linker
import GHC.Linker.Types


import GHC.Driver.Config.Diagnostic
import GHC.Driver.Pipeline
import GHC.Driver.Session
import GHC.Driver.DynFlags (ReexportedModule(..))
import GHC.Driver.Monad
import GHC.Driver.Env
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Main
import GHC.Driver.MakeSem
import GHC.Driver.Downsweep
import GHC.Driver.MakeAction

import GHC.ByteCode.Types

import GHC.Iface.Load      ( cannotFindModule, readIface )
import GHC.IfaceToCore     ( typecheckIface )
import GHC.Iface.Recomp    ( RecompileRequired(..), CompileReason(..) )

import GHC.Data.Bag        ( listToBag )
import GHC.Data.Graph.Directed
import GHC.Data.Maybe      ( expectJust )

import GHC.Utils.Exception ( throwIO, SomeAsyncException )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Error
import GHC.Utils.Logger
import GHC.Utils.TmpFs

import GHC.Types.Basic
import GHC.Types.Error
import GHC.Types.Target
import GHC.Types.SourceFile
import GHC.Types.SourceError
import GHC.Types.SrcLoc
import GHC.Types.PkgQual

import GHC.Unit
import GHC.Unit.Env
import GHC.Unit.Finder
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Graph
import GHC.Unit.Home.ModInfo
import GHC.Unit.Module.ModDetails

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Concurrent.MVar
import Control.Monad
import qualified Control.Monad.Catch as MC
import Data.IORef
import Data.Maybe
import Data.List (sortOn, groupBy, sortBy)
import System.FilePath

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.Map.Strict as M
import GHC.Types.TypeEnv
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import GHC.Driver.Env.KnotVars
import Control.Monad.Trans.Maybe
import GHC.Runtime.Loader
import GHC.Utils.Constants
import GHC.Iface.Errors.Types
import Data.Function
import qualified GHC.Data.Maybe as M

import GHC.Data.Graph.Directed.Reachability
import qualified GHC.Unit.Home.Graph as HUG
import GHC.Unit.Home.PackageTable

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

        all_errs <- liftIO $ HUG.unitEnv_foldWithKey one_unit_messages (return emptyMessages) (hsc_HUG hsc_env)
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

    (errs, mod_graph) <- liftIO $ downsweep
      hsc_env diag_wrapper msg (mgModSummaries old_graph)
      excluded_mods allow_dup_roots
    return (unionManyMessages errs, mod_graph)


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
          mapMaybe (\(_st, fs, mn) -> lookupModulePackage us (unLoc mn) fs)
            $ concatMap ms_imps home_mod_sum

        used_args = Set.fromList (map unitId loadedPackages)

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

        -- Compute the intermediate modules between a file and its hs-boot file.
        -- See Step 2a in Note [Upsweep]
        boot_path mn uid =
          Set.toList $
          -- Don't include the boot module itself
          Set.filter ((/= NodeKey_Module (key IsBoot)) . mkNodeKey)  $
          -- Keep intermediate dependencies: as per Step 2a in Note [Upsweep], these are
          -- the transitive dependencies of the non-boot file which transitively depend
          -- on the boot file.
          Set.filter (\(mkNodeKey -> nk) ->
            nodeKeyUnitId nk == uid  -- Cheap test
              && mgQuery mod_graph nk (NodeKey_Module (key IsBoot))) $
          Set.fromList $
          expectJust (mgReachable mod_graph (NodeKey_Module (key NotBoot)))
          where
            key ib = ModNodeKeyWithUid (GWIB mn ib) uid


        -- An environment mapping a module to its hs-boot file and all nodes on the path between the two, if one exists
        boot_modules = mkModuleEnv
          [ (mn, (m, boot_path (moduleName mn) (moduleUnitId mn)))
            | m@(ModuleNode _ ms) <- mgModSummaries' mod_graph
            , let mn = moduleNodeInfoModule ms
            , isBootModuleNodeInfo ms == IsBoot]

        select_boot_modules :: [ModuleGraphNode] -> [ModuleGraphNode]
        select_boot_modules = mapMaybe (fmap fst . get_boot_module)

        get_boot_module :: ModuleGraphNode -> Maybe (ModuleGraphNode, [ModuleGraphNode])
        get_boot_module (ModuleNode _ ms)
          | NotBoot <- isBootModuleNodeInfo ms
          = lookupModuleEnv boot_modules (moduleNodeInfoModule ms)
        get_boot_module _ = Nothing

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

    assertPpr (sum (map countMods build_plan) == lengthMG mod_graph)
              (vcat [text "Build plan missing nodes:", (text "PLAN:" <+> ppr (sum (map countMods build_plan))), (text "GRAPH:" <+> ppr (lengthMG mod_graph))])
              build_plan


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
                                  $ DriverModuleNotFound (moduleUnit m) (moduleName m)

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
                            [ms | (ModuleNodeCompile ms) <- (flattenSCCs (filterToposortToModules  mg2_with_srcimps))]



    -- before we unload anything, make sure we don't leave an old
    -- interactive context around pointing to dead bindings.  Also,
    -- write an empty HPT to allow the old HPT to be GC'd.

    let pruneHomeUnitEnv hme = do
          emptyHPT <- liftIO emptyHomePackageTable
          pure $! hme{ homeUnitEnv_hpt = emptyHPT }
    hug' <- traverse pruneHomeUnitEnv (ue_home_unit_graph $ hsc_unit_env hsc_env)
    let ue' = (hsc_unit_env hsc_env){ ue_home_unit_graph = hug' }
    setSession $ discardIC hsc_env{hsc_unit_env = ue' }
    hsc_env <- getSession

    -- Unload everything
    liftIO $ unload interp hsc_env

    liftIO $ debugTraceMsg logger 2 (hang (text "Ready for upsweep")
                                    2 (ppr build_plan))

    worker_limit <- liftIO $ mkWorkerLimit dflags

    (upsweep_ok, new_deps) <- withDeferredDiagnostics $ do
      hsc_env <- getSession
      liftIO $ upsweep worker_limit hsc_env mhmi_cache diag_wrapper mHscMessage (toCache pruned_cache) build_plan

    -- At this point, all the HPT variables will be populated, but we don't want
    -- to leak the contents of a failed session.
    liftIO $ restrictDepsHscEnv new_deps hsc_env
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
          flip fmap (hsc_HUG env) $ \hue ->
            let dflags = homeUnitEnv_dflags hue
                platform = targetPlatform dflags
                mainModuleSrcPath :: Maybe String
                mainModuleSrcPath = do
                  ms <- mgLookupModule mod_graph (mainModIs hue)
                  ml_hs_file (moduleNodeInfoLocation ms)
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
                , mi_src_hash iface == Just (ms_hs_hash ms)
                = linkable
                | otherwise
                = emptyHomeModInfoLinkable

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
                               , resultVar    :: ResultVar (Maybe HomeModInfo)
                               }

-- The origin of this result var, useful for debugging
data ResultOrigin = NoLoop | Loop ResultLoopOrigin deriving (Show)

data ResultLoopOrigin = Initialise | Rehydrated | Finalised deriving (Show)

mkBuildResult :: ResultOrigin -> ResultVar (Maybe HomeModInfo) -> BuildResult
mkBuildResult = BuildResult


data BuildLoopState = BuildLoopState { buildDep :: M.Map NodeKey BuildResult
                                          -- The current way to build a specific TNodeKey, without cycles this just points to
                                          -- the appropriate result of compiling a module  but with
                                          -- cycles there can be additional indirection and can point to the result of typechecking a loop
                                     , nNODE :: Int
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
  strictMap (expectJust . flip M.lookup build_map) direct_deps

type BuildM a = StateT BuildLoopState IO a




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
  ((mcycle, plans), build_map) <- runStateT (buildLoop plan) (BuildLoopState M.empty 1)
  let wait = collect_results (buildDep build_map)
  return (mcycle, plans, wait)

  where
    collect_results build_map =
      sequence (map (\br -> collect_result (resultVar br)) (M.elems build_map))
      where
        collect_result res_var = runMaybeT (waitResult res_var)

    -- Just used for an assertion
    count_mods :: BuildPlan -> Int
    count_mods (SingleModule m) = count_m m
    count_mods (ResolvedCycle ns) = length ns
    count_mods (UnresolvedCycle ns) = length ns

    count_m (UnitNode {}) = 0
    count_m _ = 1

    n_mods = sum (map count_mods plan)

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
      !build_map <- getBuildMap
      -- 1. Get the direct dependencies of this module
      let direct_deps = mgNodeDependencies False mod
          -- It's really important to force build_deps, or the whole buildMap is retained,
          -- which would retain all the result variables, preventing us from collecting them
          -- after they are no longer used.
          !build_deps = getDependencies direct_deps build_map
      !build_action <-
            case mod of
              InstantiationNode uid iu -> do
                mod_idx <- nodeId
                return $ withCurrentUnit (mgNodeUnitId mod) $ do
                  !_ <- wait_deps build_deps
                  executeInstantiationNode mod_idx n_mods hug uid iu
                  return Nothing
              ModuleNode _build_deps ms -> do
                let !old_hmi = M.lookup (mnKey ms) old_hpt
                    rehydrate_mods = mapMaybe nodeKeyModName <$> rehydrate_nodes
                mod_idx <- nodeId
                return $ withCurrentUnit (mgNodeUnitId mod) $ do
                     !_ <- wait_deps build_deps
                     hmi <- executeCompileNode mod_idx n_mods old_hmi hug rehydrate_mods ms
                     -- Write the HMI to an external cache (if one exists)
                     -- See Note [Caching HomeModInfo]
                     liftIO $ forM mhmi_cache $ \hmi_cache -> addHmiToCache hmi_cache hmi
                     -- Make sure the result is written to the HPT var
                     liftIO $ HUG.addHomeModInfoToHug hmi hug
                     return (Just hmi)
              LinkNode _nks uid -> do
                  mod_idx <- nodeId
                  return $ withCurrentUnit (mgNodeUnitId mod) $ do
                    !_ <- wait_deps build_deps
                    executeLinkNode hug (mod_idx, n_mods) uid direct_deps
                    return Nothing
              UnitNode {} -> return $ return Nothing


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
      !build_map <- getBuildMap
      res_var <- liftIO newEmptyMVar
      let loop_unit :: UnitId
          !loop_unit = nodeKeyUnitId (gwib_mod (head deps))
          !build_deps = getDependencies (map gwib_mod deps) build_map
      let loop_action = withCurrentUnit loop_unit $ do
            !_ <- wait_deps build_deps
            hsc_env <- asks hsc_env
            let mns :: [ModuleName]
                mns = mapMaybe (nodeKeyModName . gwib_mod) deps

            hmis' <- liftIO $ rehydrateAfter hsc_env mns

            checkRehydrationInvariant hmis' deps

            -- Add hydrated interfaces to global variable
            liftIO $ mapM_ (\hmi -> HUG.addHomeModInfoToHug hmi hug) hmis'
            return hmis'

      let fanout i = Just . (!! i) <$> mkResultVar res_var
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
  hscInsertHPT hmi hsc_env
  addSptEntries (hsc_env)
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
  topSortModules drop_hs_boot_nodes
    (sortBy (cmpModuleGraphNodes `on` mkNodeKey) $ mgModSummaries' module_graph)
    mb_root_mod


  where
    -- In order to get the "right" ordering
    --    Module nodes must be in reverse lexigraphic order.
    --    All modules nodes must appear before package nodes.
    --
    -- MP: This is just the ordering which the tests needed in Jan 2025, it does
    --     not arise from nature.
    --
    -- Given the current implementation of scc, the result is in
    -- The order is sensitive to the internal implementation in Data.Graph,
    -- if it changes in future then this ordering will need to be modified.
    --
    -- The SCC algorithm firstly transposes the input graph and then
    -- performs dfs on the vertices in the order which they are originally given.
    -- Therefore, if `ExternalUnit` nodes are first, the order returned will
    -- be determined by the order the dependencies are stored in the transposed graph.
    moduleGraphNodeRank :: NodeKey -> Int
    moduleGraphNodeRank k =
      case k of
        NodeKey_Unit {}         -> 0
        NodeKey_Module {}       -> 1
        NodeKey_Link {}         -> 2
        NodeKey_ExternalUnit {} -> 3

    cmpModuleGraphNodes k1 k2 = compare (moduleGraphNodeRank k1) (moduleGraphNodeRank k2)
                                  `mappend` compare k2 k1

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
            in graphFromEdgedVerticesUniq (seq root (root:allReachable (graphReachability graph) root))

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
warnUnnecessarySourceImports :: GhcMonad m => [SCC ModuleNodeInfo] -> m ()
warnUnnecessarySourceImports sccs = do
  diag_opts <- initDiagOpts <$> getDynFlags
  when (diag_wopt Opt_WarnUnusedImports diag_opts) $ do
    let check ms =
           let mods_in_this_cycle = map moduleNodeInfoModuleName ms in
           [ warn i | (ModuleNodeCompile m) <- ms, i <- ms_home_srcimps m,
                      unLoc i `notElem`  mods_in_this_cycle ]

        warn :: Located ModuleName -> MsgEnvelope GhcMessage
        warn (L loc mod) = GhcDriverMessage <$> mkPlainMsgEnvelope diag_opts
                                                  loc (DriverUnnecessarySourceImports mod)
    logDiagnostics (mkMessages $ listToBag (concatMap (check . flattenSCC) sccs))


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
          src_span = maybe noSrcSpan (mkFileSrcSpan . moduleNodeInfoLocation) (mgNodeIsModule (head path))
  where
    graph :: [Node NodeKey ModuleGraphNode]
    graph =
      [ DigraphNode
        { node_payload = ms
        , node_key = mkNodeKey ms
        , node_dependencies = mgNodeDependencies False ms
        }
      | ms <- mss
      ]

cleanCurrentModuleTempFilesMaybe :: MonadIO m => Logger -> TmpFs -> DynFlags -> m ()
cleanCurrentModuleTempFilesMaybe logger tmpfs dflags =
  if gopt Opt_KeepTmpFiles dflags
    then liftIO $ keepCurrentModuleTempFiles logger tmpfs
    else liftIO $ cleanCurrentModuleTempFiles logger tmpfs


-- | Thin each HPT variable to only contain keys from the given dependencies.
-- This is used at the end of upsweep to make sure that only completely successfully loaded
-- modules are visible for subsequent operations.
restrictDepsHscEnv :: [HomeModInfo] -> HscEnv -> IO ()
restrictDepsHscEnv deps hsc_env =
  let deps_with_unit = map (\xs -> (fst (head xs), map snd xs)) $ groupBy ((==) `on` fst) (sortOn fst (map go deps))
      hug = ue_home_unit_graph $ hsc_unit_env hsc_env
      go hmi = (hmi_unit, hmi)
        where
          hmi_mod  = mi_module (hm_iface hmi)
          hmi_unit = toUnitId (moduleUnit hmi_mod)
  in HUG.restrictHug deps_with_unit hug


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


-- | executeCompileNode interprets how --make module should compile a ModuleNode
--
-- 1. If the ModuleNode is a ModuleNodeCompile, then we first check
--    if the interface file exists and is up to date. If it is, we return those.
--    Otherwise, we compile the module and return the new HomeModInfo.
-- 2. If the ModuleNode is a ModuleNodeFixed, then we just need to load the interface
--    and artifacts from disk.

executeCompileNode :: Int
  -> Int
  -> Maybe HomeModInfo
  -> HomeUnitGraph
  -> Maybe [ModuleName] -- List of modules we need to rehydrate before compiling
  -> ModuleNodeInfo
  -> RunMakeM HomeModInfo
executeCompileNode k n !old_hmi hug mrehydrate_mods mni = do
  me@MakeEnv{..} <- ask
  -- Rehydrate any dependencies if this module had a boot file or is a signature file.
  lift $ MaybeT (withAbstractSem compile_sem $ withLoggerHsc k me $ \hsc_env -> do
     hsc_env' <- liftIO $ maybeRehydrateBefore (setHUG hug hsc_env) mni fixed_mrehydrate_mods
     case mni of
       ModuleNodeCompile mod -> executeCompileNodeWithSource hsc_env' me  mod
       ModuleNodeFixed key loc -> executeCompileNodeFixed hsc_env' me key loc
    )

  where
    fixed_mrehydrate_mods =
      case moduleNodeInfoHscSource mni of
        -- MP: It is probably a bit of a misimplementation in backpack that
        -- compiling a signature requires an knot_var for that unit.
        -- If you remove this then a lot of backpack tests fail.
        Just HsigFile -> Just []
        _        -> mrehydrate_mods

    executeCompileNodeFixed :: HscEnv -> MakeEnv -> ModNodeKeyWithUid -> ModLocation -> IO (Maybe HomeModInfo)
    executeCompileNodeFixed hsc_env MakeEnv{diag_wrapper, env_messager} mod loc =
      wrapAction diag_wrapper hsc_env $ do
        forM_ env_messager $ \hscMessage -> hscMessage hsc_env (k, n) UpToDate (ModuleNode [] (ModuleNodeFixed mod loc))
        read_result <- readIface (hsc_logger hsc_env) (hsc_dflags hsc_env) (hsc_NC hsc_env) (mnkToModule mod) (ml_hi_file loc)
        case read_result of
          M.Failed interface_err ->
            let mn = mnkModuleName mod
                err = Can'tFindInterface (BadIfaceFile interface_err) (LookingForModule (gwib_mod mn) (gwib_isBoot mn))
            in throwErrors $ singleMessage $ mkPlainErrorMsgEnvelope noSrcSpan (GhcDriverMessage (DriverInterfaceError err))
          M.Succeeded iface -> do
            details <- genModDetails hsc_env iface
            mb_object <- findObjectLinkableMaybe (mi_module iface) loc
            mb_bytecode <- loadIfaceByteCodeLazy hsc_env iface loc (md_types details)
            let hm_linkable = HomeModLinkable mb_bytecode mb_object
            return (HomeModInfo iface details hm_linkable)

    executeCompileNodeWithSource :: HscEnv -> MakeEnv -> ModSummary -> IO (Maybe HomeModInfo)
    executeCompileNodeWithSource hsc_env MakeEnv{diag_wrapper, env_messager} mod = do
     let -- Use the cached DynFlags which includes OPTIONS_GHC pragmas
         lcl_dynflags = ms_hspp_opts mod
     let lcl_hsc_env =
             -- Localise the hsc_env to use the cached flags
             hscSetFlags lcl_dynflags $
             hsc_env
     -- Compile the module, locking with a semaphore to avoid too many modules
     -- being compiled at the same time leading to high memory usage.
     wrapAction diag_wrapper lcl_hsc_env $ do
      res <- upsweep_mod lcl_hsc_env env_messager old_hmi mod k n
      cleanCurrentModuleTempFilesMaybe (hsc_logger hsc_env) (hsc_tmpfs hsc_env) lcl_dynflags
      return res


{- Rehydration, see Note [Rehydrating Modules] -}

rehydrate :: HscEnv        -- ^ The HPT in this HscEnv needs rehydrating.
          -> [HomeModInfo] -- ^ These are the modules we want to rehydrate.
          -> IO [HomeModInfo]
rehydrate hsc_env hmis = do
  debugTraceMsg logger 2 $ (
     text "Re-hydrating loop: " <+> (ppr (map (mi_module . hm_iface) hmis)))
  -- When the HPT was pure we had to tie a knot to update the ModDetails in the
  -- HPT required to update those ModDetails, but since it was made an IORef we
  -- just have to make sure the new ModDetails are "reset" so that the new
  -- modules are looked up in HPT when it is forced. If we didn't "reset" the
  -- ModDetails, modules in a loop would refer the wrong (hs-boot) definitions
  -- (as explained in Note [Rehydrating Modules]).
  mds <- initIfaceCheck (text "rehydrate") hsc_env $
            mapM (typecheckIface . hm_iface) hmis
  let new_mods = [ hmi{ hm_details = details }
                 | (hmi,details) <- zip hmis mds
                 ]
  return new_mods

  where
    logger = hsc_logger hsc_env

-- If needed, then rehydrate the necessary modules with a suitable KnotVars for the
-- module currently being compiled.
maybeRehydrateBefore :: HscEnv -> ModuleNodeInfo -> Maybe [ModuleName] -> IO HscEnv
maybeRehydrateBefore hsc_env _ Nothing = return hsc_env
maybeRehydrateBefore hsc_env mni (Just mns) = do
  knot_var <- initialise_knot_var hsc_env
  let hsc_env' = hsc_env { hsc_type_env_vars = knotVarsFromModuleEnv knot_var }
  hmis <- mapM (fmap expectJust . lookupHpt (hsc_HPT hsc_env')) mns
  hmis' <- rehydrate hsc_env' hmis
  mapM_ (\hmi -> HUG.addHomeModInfoToHug hmi (hsc_HUG hsc_env')) hmis'
  return hsc_env'

  where
   initialise_knot_var hsc_env = liftIO $
    let mod_name = homeModuleInstantiation (hsc_home_unit_maybe hsc_env) (moduleNodeInfoModule mni)
    in mkModuleEnv . (:[]) . (mod_name,) <$> newIORef emptyTypeEnv

rehydrateAfter :: HscEnv
  -> [ModuleName]
  -> IO [HomeModInfo]
rehydrateAfter hsc mns = do
  let hpt = hsc_HPT hsc
  hmis <- mapM (fmap expectJust . lookupHpt hpt) mns
  rehydrate (hsc { hsc_type_env_vars = emptyKnotVars }) hmis

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

-- | Wait for dependencies to finish, and then return their results.
wait_deps :: [BuildResult] -> RunMakeM [HomeModInfo]
wait_deps [] = return []
wait_deps (x:xs) = do
  res <- lift $ waitResult (resultVar x)
  hmis <- wait_deps xs
  case res of
    Nothing -> return hmis
    Just hmi -> return (hmi:hmis)


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
-}
