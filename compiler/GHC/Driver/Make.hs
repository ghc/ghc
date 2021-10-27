{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}

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
        load, loadWithCache, load', LoadHowMuch(..),
        instantiationNodes,

        downsweep,

        topSortModuleGraph,

        ms_home_srcimps, ms_home_imps,

        summariseModule,
        summariseFile,
        hscSourceToIsBoot,
        findExtraSigImports,
        implicitRequirementsShallow,

        noModError, cyclicModuleErr,
        SummaryNode,
        IsBootInterface(..), mkNodeKey,

        ModNodeMap(..), emptyModNodeMap, modNodeMapElems, modNodeMapLookup, modNodeMapInsert
        ) where

import GHC.Prelude
import GHC.Platform

import GHC.Tc.Utils.Backpack
import GHC.Tc.Utils.Monad  ( initIfaceCheck )

import GHC.Runtime.Interpreter
import qualified GHC.Linker.Loader as Linker
import GHC.Linker.Types

import GHC.Runtime.Context

import GHC.Driver.Config.Finder (initFinderOpts)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Phases
import GHC.Driver.Pipeline
import GHC.Driver.Session
import GHC.Driver.Backend
import GHC.Driver.Monad
import GHC.Driver.Env
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Main

import GHC.Parser.Header

import GHC.Iface.Load      ( cannotFindModule )
import GHC.IfaceToCore     ( typecheckIface )
import GHC.Iface.Recomp    ( RecompileRequired ( MustCompile ) )

import GHC.Data.Bag        ( listToBag )
import GHC.Data.Graph.Directed
import GHC.Data.FastString
import GHC.Data.Maybe      ( expectJust )
import GHC.Data.StringBuffer
import qualified GHC.LanguageExtensions as LangExt

import GHC.Utils.Exception ( throwIO, SomeAsyncException )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
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
import GHC.Types.Unique.FM
import GHC.Types.Unique.DSet
import GHC.Types.Unique.Set
import GHC.Types.Name
import GHC.Types.PkgQual

import GHC.Unit
import GHC.Unit.Env
import GHC.Unit.Finder
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.Graph
import GHC.Unit.Home.ModInfo

import Data.Either ( rights, partitionEithers )
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC.Data.FiniteMap as Map ( insertListWith )

import Control.Concurrent ( forkIO, newQSem, waitQSem, signalQSem, ThreadId, killThread, forkIOWithUnmask )
import qualified GHC.Conc as CC
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans.Except ( ExceptT(..), runExceptT, throwE )
import qualified Control.Monad.Catch as MC
import Data.IORef
import Data.Foldable (toList)
import Data.Maybe
import Data.Time
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
    (errs, mod_graph) <- depanalE excluded_mods allow_dup_roots
    if isEmptyMessages errs
      then pure mod_graph
      else throwErrors (fmap GhcDriverMessage errs)

-- | Perform dependency analysis like in 'depanal'.
-- In case of errors, the errors and an empty module graph are returned.
depanalE :: GhcMonad m =>     -- New for #17459
            [ModuleName]      -- ^ excluded modules
            -> Bool           -- ^ allow duplicate roots
            -> m (DriverMessages, ModuleGraph)
depanalE excluded_mods allow_dup_roots = do
    hsc_env <- getSession
    (errs, mod_graph) <- depanalPartial excluded_mods allow_dup_roots
    if isEmptyMessages errs
      then do
        let unused_home_mod_err = warnMissingHomeModules hsc_env mod_graph
            unused_pkg_err = warnUnusedPackages hsc_env mod_graph
        logDiagnostics (GhcDriverMessage <$> (unused_home_mod_err `unionMessages` unused_pkg_err))
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
    => [ModuleName]  -- ^ excluded modules
    -> Bool          -- ^ allow duplicate roots
    -> m (DriverMessages, ModuleGraph)
    -- ^ possibly empty 'Bag' of errors and a module graph.
depanalPartial excluded_mods allow_dup_roots = do
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
    liftIO $ flushFinderCaches (hsc_FC hsc_env) (hsc_home_unit hsc_env)

    mod_summariesE <- liftIO $ downsweep
      hsc_env (mgExtendedModSummaries old_graph)
      excluded_mods allow_dup_roots
    let
      (errs, mod_summaries) = partitionEithers mod_summariesE
      mod_graph = mkModuleGraph' $
        (instantiationNodes (hsc_units hsc_env))
        ++ fmap ModuleNode mod_summaries
    return (unionManyMessages errs, mod_graph)

-- | Collect the instantiations of dependencies to create 'InstantiationNode' work graph nodes.
-- These are used to represent the type checking that is done after
-- all the free holes (sigs in current package) relevant to that instantiation
-- are compiled. This is necessary to catch some instantiation errors.
--
-- In the future, perhaps more of the work of instantiation could be moved here,
-- instead of shoved in with the module compilation nodes. That could simplify
-- backpack, and maybe hs-boot too.
instantiationNodes :: UnitState -> [ModuleGraphNode]
instantiationNodes unit_state = InstantiationNode <$> iuids_to_check
  where
    iuids_to_check :: [InstantiatedUnit]
    iuids_to_check =
      nubSort $ concatMap goUnitId (explicitUnits unit_state)
     where
      goUnitId uid =
        [ recur
        | VirtUnit indef <- [uid]
        , inst <- instUnitInsts indef
        , recur <- (indef :) $ goUnitId $ moduleUnit $ snd inst
        ]

-- Note [Missing home modules]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Sometimes user doesn't want GHC to pick up modules, not explicitly listed
-- in a command line. For example, cabal may want to enable this warning
-- when building a library, so that GHC warns user about modules, not listed
-- neither in `exposed-modules`, nor in `other-modules`.
--
-- Here "home module" means a module, that doesn't come from an other package.
--
-- For example, if GHC is invoked with modules "A" and "B" as targets,
-- but "A" imports some other module "C", then GHC will issue a warning
-- about module "C" not being listed in a command line.
--
-- The warning in enabled by `-Wmissing-home-modules`. See #13129
warnMissingHomeModules :: HscEnv -> ModuleGraph -> DriverMessages
warnMissingHomeModules hsc_env mod_graph =
  if null missing
    then emptyMessages
    else warn
  where
    dflags = hsc_dflags hsc_env
    targets = map targetId (hsc_targets hsc_env)
    diag_opts = initDiagOpts dflags

    is_known_module mod = any (is_my_target mod) targets

    -- We need to be careful to handle the case where (possibly
    -- path-qualified) filenames (aka 'TargetFile') rather than module
    -- names are being passed on the GHC command-line.
    --
    -- For instance, `ghc --make src-exe/Main.hs` and
    -- `ghc --make -isrc-exe Main` are supposed to be equivalent.
    -- Note also that we can't always infer the associated module name
    -- directly from the filename argument.  See #13727.
    is_my_target mod (TargetModule name)
      = moduleName (ms_mod mod) == name
    is_my_target mod (TargetFile target_file _)
      | Just mod_file <- ml_hs_file (ms_location mod)
      = target_file == mod_file ||

           --  Don't warn on B.hs-boot if B.hs is specified (#16551)
           addBootSuffix target_file == mod_file ||

           --  We can get a file target even if a module name was
           --  originally specified in a command line because it can
           --  be converted in guessTarget (by appending .hs/.lhs).
           --  So let's convert it back and compare with module name
           mkModuleName (fst $ splitExtension target_file)
            == moduleName (ms_mod mod)
    is_my_target _ _ = False

    missing = map (moduleName . ms_mod) $
      filter (not . is_known_module) (mgModSummaries mod_graph)

    warn = singleMessage $ mkPlainMsgEnvelope diag_opts noSrcSpan
                         $ DriverMissingHomeModules missing (checkBuildingCabalPackage dflags)

-- | Describes which modules of the module graph need to be loaded.
data LoadHowMuch
   = LoadAllTargets
     -- ^ Load all targets and its dependencies.
   | LoadUpTo ModuleName
     -- ^ Load only the given module and its dependencies.
   | LoadDependenciesOf ModuleName
     -- ^ Load only the dependencies of the given module, but not the module
     -- itself.

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
load how_much = fst <$> loadWithCache [] how_much

loadWithCache :: GhcMonad m => [HomeModInfo] -> LoadHowMuch -> m (SuccessFlag, [HomeModInfo])
loadWithCache cache how_much = do
    (errs, mod_graph) <- depanalE [] False                        -- #17459
    success <- load' cache how_much (Just batchMsg) mod_graph
    if isEmptyMessages errs
      then pure success
      else throwErrors (fmap GhcDriverMessage errs)

-- Note [Unused packages]
--
-- Cabal passes `--package-id` flag for each direct dependency. But GHC
-- loads them lazily, so when compilation is done, we have a list of all
-- actually loaded packages. All the packages, specified on command line,
-- but never loaded, are probably unused dependencies.

warnUnusedPackages :: HscEnv -> ModuleGraph -> DriverMessages
warnUnusedPackages hsc_env mod_graph =
    let dflags = hsc_dflags hsc_env
        state  = hsc_units  hsc_env
        diag_opts = initDiagOpts dflags
        us = hsc_units hsc_env

    -- Only need non-source imports here because SOURCE imports are always HPT
        loadedPackages = concat $
          mapMaybe (\(fs, mn) -> lookupModulePackage us (unLoc mn) fs)
            $ concatMap ms_imps (mgModSummaries mod_graph)

        requestedArgs = mapMaybe packageArg (packageFlags dflags)

        unusedArgs
          = filter (\arg -> not $ any (matching state arg) loadedPackages)
                   requestedArgs

        warn = singleMessage $ mkPlainMsgEnvelope diag_opts noSrcSpan (DriverUnusedPackages unusedArgs)

    in if null unusedArgs
        then emptyMessages
        else warn

    where
        packageArg (ExposePackage _ arg _) = Just arg
        packageArg _ = Nothing

        matchingStr :: String -> UnitInfo -> Bool
        matchingStr str p
                =  str == unitPackageIdString p
                || str == unitPackageNameString p

        matching :: UnitState -> PackageArg -> UnitInfo -> Bool
        matching _ (PackageArg str) p = matchingStr str p
        matching state (UnitIdArg uid) p = uid == realUnit state p

        -- For wired-in packages, we have to unwire their id,
        -- otherwise they won't match package flags
        realUnit :: UnitState -> UnitInfo -> Unit
        realUnit state
          = unwireUnit state
          . RealUnit
          . Definite
          . unitId


-- | A ModuleGraphNode which also has a hs-boot file, and the list of nodes on any
-- path from module to its boot file.
data ModuleGraphNodeWithBootFile
  = ModuleGraphNodeWithBootFile ModuleGraphNode [ModuleGraphNode]

instance Outputable ModuleGraphNodeWithBootFile where
  ppr (ModuleGraphNodeWithBootFile mgn deps) = text "ModeGraphNodeWithBootFile: " <+> ppr mgn $$ ppr deps

getNode :: ModuleGraphNodeWithBootFile -> ModuleGraphNode
getNode (ModuleGraphNodeWithBootFile mgn _) = mgn
data BuildPlan = SingleModule ModuleGraphNode  -- A simple, single module all alone but *might* have an hs-boot file which isn't part of a cycle
               | ResolvedCycle [Either ModuleGraphNode ModuleGraphNodeWithBootFile]   -- A resolved cycle, linearised by hs-boot files
               | UnresolvedCycle [ModuleGraphNode] -- An actual cycle, which wasn't resolved by hs-boot files

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
createBuildPlan :: ModuleGraph -> Maybe ModuleName -> [BuildPlan]
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
          in acyclic ++ [maybe (UnresolvedCycle nodes) ResolvedCycle mresolved_cycle] ++ toBuildPlan sccs []

        (mg, lookup_node) = moduleGraphNodes False (mgModSummaries' mod_graph)
        trans_deps_map = allReachable mg (mkNodeKey . node_payload)
        boot_path mn =
          map (summaryNodeSummary . expectJust "toNode" . lookup_node) $ Set.toList $
          Set.delete (NodeKey_Module (GWIB mn IsBoot))  $
          expectJust "boot_path" (M.lookup (NodeKey_Module (GWIB mn NotBoot)) trans_deps_map)
            `Set.difference` (expectJust "boot_path" (M.lookup (NodeKey_Module (GWIB mn IsBoot)) trans_deps_map))


        -- An environment mapping a module to its hs-boot file and all nodes on the path between the two, if one exists
        boot_modules = mkModuleEnv
          [ (ms_mod ms, (m, boot_path (ms_mod_name ms))) | m@(ModuleNode (ExtendedModSummary ms _)) <- (mgModSummaries' mod_graph), isBootSummary ms == IsBoot]

        select_boot_modules :: [ModuleGraphNode] -> [ModuleGraphNode]
        select_boot_modules = mapMaybe (fmap fst . get_boot_module)

        get_boot_module :: (ModuleGraphNode -> Maybe (ModuleGraphNode, [ModuleGraphNode]))
        get_boot_module m = case m of ModuleNode (ExtendedModSummary ms _) | HsSrcFile <- ms_hsc_src ms -> lookupModuleEnv boot_modules (ms_mod ms); _ -> Nothing

        -- Any cycles should be resolved now
        collapseSCC :: [SCC ModuleGraphNode] -> Maybe [(Either ModuleGraphNode ModuleGraphNodeWithBootFile)]
        -- Must be at least two nodes, as we were in a cycle
        collapseSCC [AcyclicSCC node1, AcyclicSCC node2] = Just [toNodeWithBoot node1, toNodeWithBoot node2]
        collapseSCC (AcyclicSCC node : nodes) = (toNodeWithBoot node :) <$> collapseSCC nodes
        -- Cyclic
        collapseSCC _ = Nothing

        toNodeWithBoot :: (ModuleGraphNode -> Either ModuleGraphNode ModuleGraphNodeWithBootFile)
        toNodeWithBoot mn =
          case get_boot_module mn of
            -- The node doesn't have a boot file
            Nothing -> Left mn
            -- The node does have a boot file
            Just path -> Right (ModuleGraphNodeWithBootFile mn (snd path))

        -- The toposort and accumulation of acyclic modules is solely to pick-up
        -- hs-boot files which are **not** part of cycles.
        collapseAcyclic :: [SCC ModuleGraphNode] -> [BuildPlan]
        collapseAcyclic (AcyclicSCC node : nodes) = SingleModule node : collapseAcyclic nodes
        collapseAcyclic (CyclicSCC cy_nodes : nodes) = (UnresolvedCycle cy_nodes) : collapseAcyclic nodes
        collapseAcyclic [] = []

        topSortWithBoot nodes = topSortModules False (select_boot_modules nodes ++ nodes) Nothing


  in

    assertPpr (sum (map countMods build_plan) == length (mgModSummaries' mod_graph))
              (vcat [text "Build plan missing nodes:", (text "PLAN:" <+> ppr build_plan), (text "GRAPH:" <+> ppr (mgModSummaries' mod_graph ))])
              build_plan

-- | Generalized version of 'load' which also supports a custom
-- 'Messager' (for reporting progress) and 'ModuleGraph' (generally
-- produced by calling 'depanal'.
load' :: GhcMonad m => [HomeModInfo] -> LoadHowMuch -> Maybe Messager -> ModuleGraph -> m (SuccessFlag, [HomeModInfo])
load' cache how_much mHscMessage mod_graph = do
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
          mkUniqSet [ ms_mod_name s
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
            | m `elementOfUniqSet` all_home_mods = and_then
            | otherwise = do
                    liftIO $ errorMsg logger
                        (text "no such module:" <+> quotes (ppr m))
                    return (Failed, [])

    checkHowMuch how_much $ do

    -- mg2_with_srcimps drops the hi-boot nodes, returning a
    -- graph with cycles. It is just used for warning about unecessary source imports.
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




    let
        -- prune the HPT so everything is not retained when doing an
        -- upsweep.
        !pruned_cache = pruneCache cache
                            (flattenSCCs (filterToposortToModules  mg2_with_srcimps))


    -- before we unload anything, make sure we don't leave an old
    -- interactive context around pointing to dead bindings.  Also,
    -- write an empty HPT to allow the old HPT to be GC'd.
    setSession $ discardIC $ hscUpdateHPT (const emptyHomePackageTable) hsc_env

    -- Unload everything
    liftIO $ unload interp hsc_env

    liftIO $ debugTraceMsg logger 2 (hang (text "Ready for upsweep")
                                    2 (ppr build_plan))

    let direct_deps = mkDepsMap (mgModSummaries' mod_graph)

    n_jobs <- case parMakeCount dflags of
                    Nothing -> liftIO getNumProcessors
                    Just n  -> return n

    setSession $ hscUpdateHPT (const emptyHomePackageTable) hsc_env
    hsc_env <- getSession
    (upsweep_ok, hsc_env1, new_cache) <- withDeferredDiagnostics $
      liftIO $ upsweep n_jobs hsc_env mHscMessage (toCache pruned_cache) direct_deps build_plan
    setSession hsc_env1
    fmap (, new_cache) $ case upsweep_ok of
      Failed -> loadFinish upsweep_ok Succeeded

      Succeeded -> do
       -- Make modsDone be the summaries for each home module now
       -- available; this should equal the domain of hpt3.
       -- Get in in a roughly top .. bottom order (hence reverse).

       -- Try and do linking in some form, depending on whether the
       -- upsweep was completely or only partially successful.

       -- Easy; just relink it all.
       do liftIO $ debugTraceMsg logger 2 (text "Upsweep completely successful.")

          -- Clean up after ourselves
          hsc_env1 <- getSession
          liftIO $ cleanCurrentModuleTempFilesMaybe logger (hsc_tmpfs hsc_env1) dflags

          -- Issue a warning for the confusing case where the user
          -- said '-o foo' but we're not going to do any linking.
          -- We attempt linking if either (a) one of the modules is
          -- called Main, or (b) the user said -no-hs-main, indicating
          -- that main() is going to come from somewhere else.
          --
          let ofile = outputFile_ dflags
          let no_hs_main = gopt Opt_NoHsMain dflags
          let
            main_mod = mainModIs hsc_env
            a_root_is_Main = mgElemModule mod_graph main_mod
            do_linking = a_root_is_Main || no_hs_main || ghcLink dflags == LinkDynLib || ghcLink dflags == LinkStaticLib

          -- link everything together
          hsc_env <- getSession
          linkresult <- liftIO $ link (ghcLink dflags)
                                      logger
                                      (hsc_tmpfs hsc_env)
                                      (hsc_hooks hsc_env)
                                      dflags
                                      (hsc_unit_env hsc_env)
                                      do_linking
                                      (hsc_HPT hsc_env1)

          if ghcLink dflags == LinkBinary && isJust ofile && not do_linking
             then do
                liftIO $ errorMsg logger $ text
                   ("output was redirected with -o, " ++
                    "but no output will be generated\n" ++
                    "because there is no " ++
                    moduleNameString (moduleName main_mod) ++ " module.")
                -- This should be an error, not a warning (#10895).
                loadFinish Failed linkresult
             else
                loadFinish Succeeded linkresult

partitionNodes
  :: [ModuleGraphNode]
  -> ( [InstantiatedUnit]
     , [ExtendedModSummary]
     )
partitionNodes ns = partitionEithers $ flip fmap ns $ \case
  InstantiationNode x -> Left x
  ModuleNode x -> Right x

-- | Finish up after a load.
loadFinish :: GhcMonad m => SuccessFlag -> SuccessFlag -> m SuccessFlag

-- If the link failed, unload everything and return.
loadFinish _all_ok Failed
  = do hsc_env <- getSession
       let interp = hscInterp hsc_env
       liftIO $ unload interp hsc_env
       modifySession discardProg
       return Failed

-- Empty the interactive context and set the module context to the topmost
-- newly loaded module, or the Prelude if none were loaded.
loadFinish all_ok Succeeded
  = do modifySession discardIC
       return all_ok


-- | Forget the current program, but retain the persistent info in HscEnv
discardProg :: HscEnv -> HscEnv
discardProg hsc_env
  = discardIC
    $ hscUpdateHPT (const emptyHomePackageTable)
    $ hsc_env { hsc_mod_graph = emptyMG }

-- | Discard the contents of the InteractiveContext, but keep the DynFlags and
-- the loaded plugins.  It will also keep ic_int_print and ic_monad if their
-- names are from external packages.
discardIC :: HscEnv -> HscEnv
discardIC hsc_env
  = hsc_env { hsc_IC = empty_ic { ic_int_print = new_ic_int_print
                                , ic_monad     = new_ic_monad
                                , ic_plugins   = old_plugins
                                } }
  where
  -- Force the new values for ic_int_print and ic_monad to avoid leaking old_ic
  !new_ic_int_print = keep_external_name ic_int_print
  !new_ic_monad = keep_external_name ic_monad
  !old_plugins = ic_plugins old_ic
  dflags = ic_dflags old_ic
  old_ic = hsc_IC hsc_env
  empty_ic = emptyInteractiveContext dflags
  keep_external_name ic_name
    | nameIsFromExternalPackage home_unit old_name = old_name
    | otherwise = ic_name empty_ic
    where
    home_unit = hsc_home_unit hsc_env
    old_name = ic_name old_ic

-- | If there is no -o option, guess the name of target executable
-- by using top-level source file name as a base.
guessOutputFile :: GhcMonad m => m ()
guessOutputFile = modifySession $ \env ->
    let dflags = hsc_dflags env
        platform = targetPlatform dflags
        -- Force mod_graph to avoid leaking env
        !mod_graph = hsc_mod_graph env
        mainModuleSrcPath :: Maybe String
        mainModuleSrcPath = do
            ms <- mgLookupModule mod_graph (mainModIs env)
            ml_hs_file (ms_location ms)
        name = fmap dropExtension mainModuleSrcPath

        !name_exe = do
          -- we must add the .exe extension unconditionally here, otherwise
          -- when name has an extension of its own, the .exe extension will
          -- not be added by GHC.Driver.Pipeline.exeFileName.  See #2248
          !name' <- if platformOS platform == OSMinGW32
                    then fmap (<.> "exe") name
                    else name
          mainModuleSrcPath' <- mainModuleSrcPath
          -- #9930: don't clobber input files (unless they ask for it)
          if name' == mainModuleSrcPath'
            then throwGhcException . UsageError $
                 "default output name would overwrite the input file; " ++
                 "must specify -o explicitly"
            else Just name'
    in
    case outputFile_ dflags of
        Just _ -> env
        Nothing -> hscSetFlags (dflags { outputFile_ = name_exe }) env

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
pruneCache :: [HomeModInfo]
                      -> [ModSummary]
                      -> [HomeModInfo]
pruneCache hpt summ
  = strictMap prune hpt
  where prune hmi = hmi'{ hm_details = emptyModDetails }
          where
           modl = moduleName (mi_module (hm_iface hmi))
           hmi' | Just ms <- lookupUFM ms_map modl
                , mi_src_hash (hm_iface hmi) /= ms_hs_hash ms
                = hmi{ hm_linkable = Nothing }
                | otherwise
                = hmi

        ms_map = listToUFM [(ms_mod_name ms, ms) | ms <- summ]

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
* The amount of parrelism is controlled by a semaphore. This is just used around the
  module compilation step, so that only the right number of modules are compiled at
  the same time which reduces overal memory usage and allocations.
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
         modules on the path between it and the hs-boot file. This information is
         stored in ModuleGraphNodeWithBoot.

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

instance Functor ResultVar where
  fmap f (ResultVar g var) = ResultVar (f . g) var

mkResultVar :: MVar (Maybe a) -> ResultVar a
mkResultVar = ResultVar id

-- | Block until the result is ready.
waitResult :: ResultVar a -> MaybeT IO a
waitResult (ResultVar f var) = MaybeT (fmap f <$> readMVar var)


data BuildLoopState = BuildLoopState { buildDep :: M.Map NodeKey (SDoc, ResultVar (Maybe HomeModInfo))
                                          -- The current way to build a specific TNodeKey, without cycles this just points to
                                          -- the appropiate result of compiling a module  but with
                                          -- cycles there can be additional indirection and can point to the result of typechecking a loop
                                     , nNODE :: Int
                                     , hpt_var :: MVar HomePackageTable
                                     -- A global variable which is incrementally updated with the result
                                     -- of compiling modules.
                                     }

nodeId :: BuildM Int
nodeId = do
  n <- gets nNODE
  modify (\m -> m { nNODE = n + 1 })
  return n

setModulePipeline :: NodeKey -> SDoc -> ResultVar (Maybe HomeModInfo) -> BuildM ()
setModulePipeline mgn doc wrapped_pipeline = do
  modify (\m -> m { buildDep = M.insert mgn (doc, wrapped_pipeline) (buildDep m) })

getBuildMap :: BuildM (M.Map
                    NodeKey (SDoc, ResultVar (Maybe HomeModInfo)))
getBuildMap = gets buildDep

type BuildM a = StateT BuildLoopState IO a


-- | Abstraction over the operations of a semaphore which allows usage with the
--  -j1 case
data AbstractSem = AbstractSem { acquireSem :: IO ()
                               , releaseSem :: IO () }

withAbstractSem :: AbstractSem -> IO b -> IO b
withAbstractSem sem = MC.bracket_ (acquireSem sem) (releaseSem sem)

-- | Environment used when compiling a module
data MakeEnv = MakeEnv { hsc_env :: !HscEnv -- The basic HscEnv which will be augmented for each module
                       , compile_sem :: !AbstractSem
                       -- Modify the environment for module k, with the supplied logger modification function.
                       -- For -j1, this wrapper doesn't do anything
                       -- For -jn, the wrapper initialised a log queue and then modifies the logger to pipe its output
                       --          into the log queue.
                       , withLogger :: forall a . Int -> ((Logger -> Logger) -> RunMakeM a) -> RunMakeM a
                       , env_messager :: !(Maybe Messager)
                       }

type RunMakeM a = ReaderT MakeEnv (MaybeT IO) a

-- | Given the build plan, creates a graph which indicates where each NodeKey should
-- get its direct dependencies from. This might not be the corresponding build action
-- if the module participates in a loop. This step also labels each node with a number for the output.
-- See Note [Upsweep] for a high-level description.
interpretBuildPlan :: (M.Map ModuleNameWithIsBoot HomeModInfo)
                   -> (NodeKey -> [NodeKey])
                   -> [BuildPlan]
                   -> IO ( Maybe [ModuleGraphNode] -- Is there an unresolved cycle
                         , [MakeAction] -- Actions we need to run in order to build everything
                         , IO [Maybe (Maybe HomeModInfo)]) -- An action to query to get all the built modules at the end.
interpretBuildPlan old_hpt deps_map plan = do
  hpt_var <- newMVar emptyHomePackageTable
  ((mcycle, plans), build_map) <- runStateT (buildLoop plan) (BuildLoopState M.empty 1 hpt_var)
  return (mcycle, plans, collect_results (buildDep build_map))

  where
    collect_results build_map = mapM (\(_doc, res_var) -> runMaybeT (waitResult res_var)) (M.elems build_map)

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
          (one_plan, _) <- buildSingleModule Nothing m
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

    buildSingleModule :: Maybe [ModuleGraphNode]  -- Modules we need to rehydrate before compiling this module
                      -> ModuleGraphNode          -- The node we are compiling
                      -> BuildM (MakeAction, ResultVar (Maybe HomeModInfo))
    buildSingleModule rehydrate_nodes mod = do
      mod_idx <- nodeId
      home_mod_map <- getBuildMap
      hpt_var <- gets hpt_var
      -- 1. Get the transitive dependencies of this module, by looking up in the dependency map
      let direct_deps = deps_map (mkNodeKey mod)
          doc_build_deps = catMaybes $ map (flip M.lookup home_mod_map) direct_deps
          build_deps = map snd doc_build_deps
      -- 2. Set the default way to build this node, not in a loop here
      let build_action = do
            hsc_env <- asks hsc_env
            case mod of
              InstantiationNode iu -> const Nothing <$> executeInstantiationNode mod_idx n_mods (wait_deps_hpt hpt_var build_deps) iu
              ModuleNode ms -> do
                  let !old_hmi = M.lookup (msKey $ emsModSummary ms) old_hpt
                      rehydrate_mods = mapMaybe moduleGraphNodeModule <$> rehydrate_nodes
                  hmi <- executeCompileNode mod_idx n_mods old_hmi (wait_deps_hpt hpt_var build_deps) rehydrate_mods (emsModSummary ms)
                  -- This global MVar is incrementally modified in order to avoid having to
                  -- recreate the HPT before compiling each module which leads to a quadratic amount of work.
                  hmi' <- liftIO $ modifyMVar hpt_var (\hpt -> do
                    let new_hpt = addHomeModInfoToHpt hmi hpt
                        new_hsc = setHPT new_hpt hsc_env
                    maybeRehydrateAfter hmi new_hsc rehydrate_mods
                      )
                  return (Just hmi')

      res_var <- liftIO newEmptyMVar
      let result_var = mkResultVar res_var
      setModulePipeline (mkNodeKey mod) (text "N") result_var
      return $ (MakeAction build_action res_var, result_var)


    buildOneLoopyModule ::  ModuleGraphNodeWithBootFile -> BuildM (MakeAction, (ResultVar (Maybe HomeModInfo)))
    buildOneLoopyModule (ModuleGraphNodeWithBootFile mn deps) =
      buildSingleModule (Just deps) mn

    buildModuleLoop :: [(Either ModuleGraphNode ModuleGraphNodeWithBootFile)] ->  BuildM [MakeAction]
    buildModuleLoop ms = do
      (build_modules, wait_modules) <- mapAndUnzipM (either (buildSingleModule Nothing) buildOneLoopyModule) ms
      res_var <- liftIO newEmptyMVar
      let loop_action = wait_deps wait_modules
      let fanout i = Just . (!! i) <$> mkResultVar res_var
      -- From outside the module loop, anyone must wait for the loop to finish and then
      -- use the result of the rehydrated iface. This makes sure that things not in the
      -- module loop will see the updated interfaces for all the identifiers in the loop.
      let update_module_pipeline (m, i) = setModulePipeline (NodeKey_Module m) (text "T") (fanout i)

      let ms_i = zip (mapMaybe (fmap (msKey . emsModSummary) . moduleGraphNodeModSum . either id getNode) ms) [0..]
      mapM update_module_pipeline ms_i
      return $ build_modules ++ [MakeAction loop_action res_var]


upsweep
    :: Int -- ^ The number of workers we wish to run in parallel
    -> HscEnv -- ^ The base HscEnv, which is augmented for each module
    -> Maybe Messager
    -> M.Map ModuleNameWithIsBoot HomeModInfo
    -> (NodeKey -> [NodeKey]) -- A function which computes the direct dependencies of a NodeKey
    -> [BuildPlan]
    -> IO (SuccessFlag, HscEnv, [HomeModInfo])
upsweep n_jobs hsc_env mHscMessage old_hpt direct_deps build_plan = do
    (cycle, pipelines, collect_result) <- interpretBuildPlan old_hpt direct_deps build_plan
    runPipelines n_jobs hsc_env mHscMessage pipelines
    res <- collect_result

    let completed = [m | Just (Just m) <- res]
    let hsc_env' = addDepsToHscEnv completed hsc_env

    -- Handle any cycle in the original compilation graph and return the result
    -- of the upsweep.
    case cycle of
        Just mss -> do
          let logger = hsc_logger hsc_env
          liftIO $ fatalErrorMsg logger (cyclicModuleErr mss)
          return (Failed, hsc_env, completed)
        Nothing  -> do
          let success_flag = successIf (all isJust res)
          return (success_flag, hsc_env', completed)

toCache :: [HomeModInfo] -> M.Map ModuleNameWithIsBoot HomeModInfo
toCache hmis = M.fromList ([(mi_mnwib $ hm_iface hmi, hmi) | hmi <- hmis])

upsweep_inst :: HscEnv
             -> Maybe Messager
             -> Int  -- index of module
             -> Int  -- total number of modules
             -> InstantiatedUnit
             -> IO ()
upsweep_inst hsc_env mHscMessage mod_index nmods iuid = do
        case mHscMessage of
            Just hscMessage -> hscMessage hsc_env (mod_index, nmods) MustCompile (InstantiationNode iuid)
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
          mod_index nmods (hm_iface <$> old_hmi) (old_hmi >>= hm_linkable)

  -- MP: This is a bit janky, because before you add the entries you have to extend the HPT with the module
  -- you just compiled. Another option, would be delay adding anything until after upsweep has finished, but I
  -- am unsure if this is sound (wrt running TH splices for example).
  -- This function only does anything if the linkable produced is a BCO, which only happens with the
  -- bytecode backend, no need to guard against the backend type additionally.
  addSptEntries (hscUpdateHPT (\hpt -> addToHpt hpt (ms_mod_name summary) hmi) hsc_env)
                (ms_mnwib summary)
                (hm_linkable hmi)

  return hmi

-- | Add the entries from a BCO linkable to the SPT table, see
-- See Note [Grand plan for static forms] in GHC.Iface.Tidy.StaticPtrTable.
addSptEntries :: HscEnv -> ModuleNameWithIsBoot -> Maybe Linkable -> IO ()
addSptEntries hsc_env mnwib mlinkable =
  hscAddSptEntries hsc_env (Just mnwib)
     [ spt
     | Just linkable <- [mlinkable]
     , unlinked <- linkableUnlinked linkable
     , BCOs _ spts <- pure unlinked
     , spt <- spts
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

The flavour of generated object code is chosen by defaultObjectTarget for the
target platform. It would likely be faster to generate bytecode, but this is not
supported on all platforms(?Please Confirm?), and does not support the entirety
of GHC haskell. See #1257.

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
          -> Maybe ModuleName
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

topSortModules :: Bool -> [ModuleGraphNode] -> Maybe ModuleName -> [SCC ModuleGraphNode]
topSortModules drop_hs_boot_nodes summaries mb_root_mod
  = map (fmap summaryNodeSummary) $ stronglyConnCompG initial_graph
  where
    (graph, lookup_node) =
      moduleGraphNodes drop_hs_boot_nodes summaries

    initial_graph = case mb_root_mod of
        Nothing -> graph
        Just root_mod ->
            -- restrict the graph to just those modules reachable from
            -- the specified module.  We do this by building a graph with
            -- the full set of nodes, and determining the reachable set from
            -- the specified node.
            let root | Just node <- lookup_node $ NodeKey_Module $ GWIB root_mod NotBoot
                     , graph `hasVertexG` node
                     = node
                     | otherwise
                     = throwGhcException (ProgramError "module does not exist")
            in graphFromEdgedVerticesUniq (seq root (reachableG graph root))

-- The nodes of the graph are keyed by (mod, is boot?) pairs for the current
-- modules, and indefinite unit IDs for dependencies which are instantiated with
-- our holes.
--
-- NB: hsig files show up as *normal* nodes (not boot!), since they don't
-- participate in cycles (for now)

mkNodeMap :: [ExtendedModSummary] -> ModNodeMap ExtendedModSummary
mkNodeMap summaries = ModNodeMap $ Map.fromList
  [ (ms_mnwib $ emsModSummary s, s) | s <- summaries]

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

-- | Efficiently construct a map from a NodeKey to its list of transitive dependencies
mkDepsMap :: [ModuleGraphNode] -> (NodeKey -> [NodeKey])
mkDepsMap nodes =
  -- Important that we force this before returning a lambda so we can share the module graph
  -- for each node
  let !(mg, lookup_node) = moduleGraphNodes False nodes
  in \nk -> map (mkNodeKey . node_payload) $ outgoingG mg (expectJust "mkDepsMap" (lookup_node nk))

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
          -> [ExtendedModSummary]
          -- ^ Old summaries
          -> [ModuleName]       -- Ignore dependencies on these; treat
                                -- them as if they were package modules
          -> Bool               -- True <=> allow multiple targets to have
                                --          the same module name; this is
                                --          very useful for ghc -M
          -> IO [Either DriverMessages ExtendedModSummary]
                -- The non-error elements of the returned list all have distinct
                -- (Modules, IsBoot) identifiers, unless the Bool is true in
                -- which case there can be repeats
downsweep hsc_env old_summaries excl_mods allow_dup_roots
   = do
       rootSummaries <- mapM getRootSummary roots
       let (errs, rootSummariesOk) = partitionEithers rootSummaries -- #17549
           root_map = mkRootMap rootSummariesOk
       checkDuplicates root_map
       map0 <- loop (concatMap calcDeps rootSummariesOk) root_map
       -- if we have been passed -fno-code, we enable code generation
       -- for dependencies of modules that have -XTemplateHaskell,
       -- otherwise those modules will fail to compile.
       -- See Note [-fno-code mode] #8025
       let default_backend = platformDefaultBackend (targetPlatform dflags)
       let home_unit       = hsc_home_unit hsc_env
       let tmpfs           = hsc_tmpfs     hsc_env
       map1 <- case backend dflags of
         NoBackend   -> enableCodeGenForTH logger tmpfs home_unit default_backend map0
         _           -> return map0
       if null errs
         then pure $ concat $ modNodeMapElems map1
         else pure $ map Left errs
     where
        -- TODO(@Ericson2314): Probably want to include backpack instantiations
        -- in the map eventually for uniformity
        calcDeps (ExtendedModSummary ms _bkp_deps) = msDeps ms

        dflags = hsc_dflags hsc_env
        logger = hsc_logger hsc_env
        roots  = hsc_targets hsc_env

        old_summary_map :: ModNodeMap ExtendedModSummary
        old_summary_map = mkNodeMap old_summaries

        getRootSummary :: Target -> IO (Either DriverMessages ExtendedModSummary)
        getRootSummary Target { targetId = TargetFile file mb_phase
                              , targetContents = maybe_buf
                              }
           = do exists <- liftIO $ doesFileExist file
                if exists || isJust maybe_buf
                    then summariseFile hsc_env old_summaries file mb_phase
                                       maybe_buf
                    else return $ Left $ singleMessage
                                $ mkPlainErrorMsgEnvelope noSrcSpan (DriverFileNotFound file)
        getRootSummary Target { targetId = TargetModule modl
                              , targetContents = maybe_buf
                              }
           = do maybe_summary <- summariseModule hsc_env old_summary_map NotBoot
                                           (L rootLoc modl)
                                           maybe_buf excl_mods
                case maybe_summary of
                   Nothing -> return $ Left $ moduleNotFoundErr modl
                   Just s  -> return s

        rootLoc = mkGeneralSrcSpan (fsLit "<command line>")

        -- In a root module, the filename is allowed to diverge from the module
        -- name, so we have to check that there aren't multiple root files
        -- defining the same module (otherwise the duplicates will be silently
        -- ignored, leading to confusing behaviour).
        checkDuplicates
          :: ModNodeMap
               [Either DriverMessages
                       ExtendedModSummary]
          -> IO ()
        checkDuplicates root_map
           | allow_dup_roots = return ()
           | null dup_roots  = return ()
           | otherwise       = liftIO $ multiRootsErr (emsModSummary <$> head dup_roots)
           where
             dup_roots :: [[ExtendedModSummary]]        -- Each at least of length 2
             dup_roots = filterOut isSingleton $ map rights $ modNodeMapElems root_map

        loop :: [GenWithIsBoot (Located ModuleName)]
                        -- Work list: process these modules
             -> ModNodeMap [Either DriverMessages ExtendedModSummary]
                        -- Visited set; the range is a list because
                        -- the roots can have the same module names
                        -- if allow_dup_roots is True
             -> IO (ModNodeMap [Either DriverMessages ExtendedModSummary])
                        -- The result is the completed NodeMap
        loop [] done = return done
        loop (s : ss) done
          | Just summs <- modNodeMapLookup key done
          = if isSingleton summs then
                loop ss done
            else
                do { multiRootsErr (emsModSummary <$> rights summs)
                   ; return (ModNodeMap Map.empty)
                   }
          | otherwise
          = do mb_s <- summariseModule hsc_env old_summary_map
                                       is_boot wanted_mod
                                       Nothing excl_mods
               case mb_s of
                   Nothing -> loop ss done
                   Just (Left e) -> loop ss (modNodeMapInsert key [Left e] done)
                   Just (Right s)-> do
                     new_map <-
                       loop (calcDeps s) (modNodeMapInsert key [Right s] done)
                     loop ss new_map
          where
            GWIB { gwib_mod = L loc mod, gwib_isBoot = is_boot } = s
            wanted_mod = L loc mod
            key = GWIB
                    { gwib_mod = unLoc wanted_mod
                    , gwib_isBoot = is_boot
                    }

-- | Update the every ModSummary that is depended on
-- by a module that needs template haskell. We enable codegen to
-- the specified target, disable optimization and change the .hi
-- and .o file locations to be temporary files.
-- See Note [-fno-code mode]
enableCodeGenForTH
  :: Logger
  -> TmpFs
  -> HomeUnit
  -> Backend
  -> ModNodeMap [Either DriverMessages ExtendedModSummary]
  -> IO (ModNodeMap [Either DriverMessages ExtendedModSummary])
enableCodeGenForTH logger tmpfs home_unit =
  enableCodeGenWhen logger tmpfs condition should_modify TFL_CurrentModule TFL_GhcSession
  where
    condition = isTemplateHaskellOrQQNonBoot
    should_modify (ModSummary { ms_hspp_opts = dflags }) =
      backend dflags == NoBackend &&
      -- Don't enable codegen for TH on indefinite packages; we
      -- can't compile anything anyway! See #16219.
      isHomeUnitDefinite home_unit

-- | Helper used to implement 'enableCodeGenForTH'.
-- In particular, this enables
-- unoptimized code generation for all modules that meet some
-- condition (first parameter), or are dependencies of those
-- modules. The second parameter is a condition to check before
-- marking modules for code generation.
enableCodeGenWhen
  :: Logger
  -> TmpFs
  -> (ModSummary -> Bool)
  -> (ModSummary -> Bool)
  -> TempFileLifetime
  -> TempFileLifetime
  -> Backend
  -> ModNodeMap [Either DriverMessages ExtendedModSummary]
  -> IO (ModNodeMap [Either DriverMessages ExtendedModSummary])
enableCodeGenWhen logger tmpfs condition should_modify staticLife dynLife bcknd nodemap =
  traverse (traverse (traverse enable_code_gen)) nodemap
  where
    enable_code_gen :: ExtendedModSummary -> IO ExtendedModSummary
    enable_code_gen (ExtendedModSummary ms bkp_deps)
      | ModSummary
        { ms_mod = ms_mod
        , ms_location = ms_location
        , ms_hsc_src = HsSrcFile
        , ms_hspp_opts = dflags
        } <- ms
      , should_modify ms
      , ms_mod `Set.member` needs_codegen_set
      = do
        let new_temp_file suf dynsuf = do
              tn <- newTempName logger tmpfs (tmpDir dflags) staticLife suf
              let dyn_tn = tn -<.> dynsuf
              addFilesToClean tmpfs dynLife [dyn_tn]
              return (tn, dyn_tn)
          -- We don't want to create .o or .hi files unless we have been asked
          -- to by the user. But we need them, so we patch their locations in
          -- the ModSummary with temporary files.
          --
        ((hi_file, dyn_hi_file), (o_file, dyn_o_file)) <-
          -- If ``-fwrite-interface` is specified, then the .o and .hi files
          -- are written into `-odir` and `-hidir` respectively.  #16670
          if gopt Opt_WriteInterface dflags
            then return ((ml_hi_file ms_location, ml_dyn_hi_file ms_location)
                        , (ml_obj_file ms_location, ml_dyn_obj_file ms_location))
            else (,) <$> (new_temp_file (hiSuf_ dflags) (dynHiSuf_ dflags))
                     <*> (new_temp_file (objectSuf_ dflags) (dynObjectSuf_ dflags))
        let ms' = ms
              { ms_location =
                  ms_location { ml_hi_file = hi_file
                              , ml_obj_file = o_file
                              , ml_dyn_hi_file = dyn_hi_file
                              , ml_dyn_obj_file = dyn_o_file }
              , ms_hspp_opts = updOptLevel 0 $ dflags {backend = bcknd}
              }
        pure (ExtendedModSummary ms' bkp_deps)
      | otherwise = return (ExtendedModSummary ms bkp_deps)

    needs_codegen_set = transitive_deps_set
      [ ms
      | mss <- modNodeMapElems nodemap
      , Right (ExtendedModSummary { emsModSummary = ms }) <- mss
      , condition ms
      ]

    -- find the set of all transitive dependencies of a list of modules.
    transitive_deps_set :: [ModSummary] -> Set.Set Module
    transitive_deps_set modSums = foldl' go Set.empty modSums
      where
        go marked_mods ms@ModSummary{ms_mod}
          | ms_mod `Set.member` marked_mods = marked_mods
          | otherwise =
            let deps =
                  [ dep_ms
                  -- If a module imports a boot module, msDeps helpfully adds a
                  -- dependency to that non-boot module in it's result. This
                  -- means we don't have to think about boot modules here.
                  | dep <- msDeps ms
                  , NotBoot == gwib_isBoot dep
                  , dep_ms_0 <- toList $ modNodeMapLookup (unLoc <$> dep) nodemap
                  , dep_ms_1 <- toList $ dep_ms_0
                  , (ExtendedModSummary { emsModSummary = dep_ms }) <- toList $ dep_ms_1
                  ]
                new_marked_mods = Set.insert ms_mod marked_mods
            in foldl' go new_marked_mods deps

mkRootMap
  :: [ExtendedModSummary]
  -> ModNodeMap [Either DriverMessages ExtendedModSummary]
mkRootMap summaries = ModNodeMap $ Map.insertListWith
  (flip (++))
  [ (msKey $ emsModSummary s, [Right s]) | s <- summaries ]
  Map.empty

-- | Returns the dependencies of the ModSummary s.
-- A wrinkle is that for a {-# SOURCE #-} import we return
--      *both* the hs-boot file
--      *and* the source file
-- as "dependencies".  That ensures that the list of all relevant
-- modules always contains B.hs if it contains B.hs-boot.
-- Remember, this pass isn't doing the topological sort.  It's
-- just gathering the list of all relevant ModSummaries
msDeps :: ModSummary -> [GenWithIsBoot (Located ModuleName)]
msDeps s = [ d
           | m <- ms_home_srcimps s
           , d <- [ GWIB { gwib_mod = m, gwib_isBoot = IsBoot }
                  , GWIB { gwib_mod = m, gwib_isBoot = NotBoot }
                  ]
           ]
        ++ [ GWIB { gwib_mod = m, gwib_isBoot = NotBoot }
           | m <- ms_home_imps s
           ]

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
        -> [ExtendedModSummary]         -- old summaries
        -> FilePath                     -- source file name
        -> Maybe Phase                  -- start phase
        -> Maybe (StringBuffer,UTCTime)
        -> IO (Either DriverMessages ExtendedModSummary)

summariseFile hsc_env old_summaries src_fn mb_phase maybe_buf
        -- we can use a cached summary if one is available and the
        -- source file hasn't changed,  But we have to look up the summary
        -- by source file, rather than module name as we do in summarise.
   | Just old_summary <- findSummaryBySourceFile old_summaries src_fn
   = do
        let location = ms_location $ emsModSummary old_summary

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
    -- src_fn does not necessarily exist on the filesystem, so we need to
    -- check what kind of target we are dealing with
    get_src_hash = case maybe_buf of
                      Just (buf,_) -> return $ fingerprintStringBuffer buf
                      Nothing -> liftIO $ getFileHash src_fn

    new_summary src_fn src_hash = runExceptT $ do
        preimps@PreprocessedImports {..}
            <- getPreprocessedImports hsc_env src_fn mb_phase maybe_buf

        let fopts = initFinderOpts (hsc_dflags hsc_env)

        -- Make a ModLocation for this file
        let location = mkHomeModLocation fopts pi_mod_name src_fn

        -- Tell the Finder cache where it is, so that subsequent calls
        -- to findModule will find it, even if it's not on any search path
        mod <- liftIO $ do
          let home_unit = hsc_home_unit hsc_env
          let fc        = hsc_FC hsc_env
          addHomeModuleToFinder fc home_unit pi_mod_name location

        liftIO $ makeNewModSummary hsc_env $ MakeNewModSummary
            { nms_src_fn = src_fn
            , nms_src_hash = src_hash
            , nms_is_boot = NotBoot
            , nms_hsc_src =
                if isHaskellSigFilename src_fn
                   then HsigFile
                   else HsSrcFile
            , nms_location = location
            , nms_mod = mod
            , nms_preimps = preimps
            }

findSummaryBySourceFile :: [ExtendedModSummary] -> FilePath -> Maybe ExtendedModSummary
findSummaryBySourceFile summaries file = case
    [ ms
    | ms <- summaries
    , HsSrcFile <- [ms_hsc_src $ emsModSummary ms]
    , let derived_file = ml_hs_file $ ms_location $ emsModSummary ms
    , expectJust "findSummaryBySourceFile" derived_file == file
    ]
  of
    [] -> Nothing
    (x:_) -> Just x

checkSummaryHash
    :: HscEnv
    -> (Fingerprint -> IO (Either e ExtendedModSummary))
    -> ExtendedModSummary -> ModLocation -> Fingerprint
    -> IO (Either e ExtendedModSummary)
checkSummaryHash
  hsc_env new_summary
  (ExtendedModSummary { emsModSummary = old_summary, emsInstantiatedUnits = bkp_deps})
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
           _ <- do
              let home_unit = hsc_home_unit hsc_env
              let fc        = hsc_FC hsc_env
              addHomeModuleToFinder fc home_unit
                  (moduleName (ms_mod old_summary)) location

           hi_timestamp <- modificationTimeIfExists (ml_hi_file location)
           hie_timestamp <- modificationTimeIfExists (ml_hie_file location)

           return $ Right
             ( ExtendedModSummary { emsModSummary = old_summary
                     { ms_obj_date = obj_timestamp
                     , ms_iface_date = hi_timestamp
                     , ms_hie_date = hie_timestamp
                     }
                   , emsInstantiatedUnits = bkp_deps
                   }
             )

   | otherwise =
           -- source changed: re-summarise.
           new_summary src_hash

-- Summarise a module, and pick up source and timestamp.
summariseModule
          :: HscEnv
          -> ModNodeMap ExtendedModSummary
          -- ^ Map of old summaries
          -> IsBootInterface    -- True <=> a {-# SOURCE #-} import
          -> Located ModuleName -- Imported module to be summarised
          -> Maybe (StringBuffer, UTCTime)
          -> [ModuleName]               -- Modules to exclude
          -> IO (Maybe (Either DriverMessages ExtendedModSummary))      -- Its new summary

summariseModule hsc_env old_summary_map is_boot (L loc wanted_mod)
                maybe_buf excl_mods
  | wanted_mod `elem` excl_mods
  = return Nothing

  | Just old_summary <- modNodeMapLookup
      (GWIB { gwib_mod = wanted_mod, gwib_isBoot = is_boot })
      old_summary_map
  = do          -- Find its new timestamp; all the
                -- ModSummaries in the old map have valid ml_hs_files
        let location = ms_location $ emsModSummary old_summary
            src_fn = expectJust "summariseModule" (ml_hs_file location)

                -- check the hash on the source file, and
                -- return the cached summary if it hasn't changed.  If the
                -- file has disappeared, we need to call the Finder again.
        case maybe_buf of
           Just (buf,_) ->
               Just <$> check_hash old_summary location src_fn (fingerprintStringBuffer buf)
           Nothing    -> do
                mb_hash <- fileHashIfExists src_fn
                case mb_hash of
                   Just hash -> Just <$> check_hash old_summary location src_fn hash
                   Nothing   -> find_it

  | otherwise  = find_it
  where
    dflags     = hsc_dflags hsc_env
    fopts      = initFinderOpts dflags
    mhome_unit = ue_home_unit (hsc_unit_env hsc_env)
    fc         = hsc_FC hsc_env
    units      = hsc_units hsc_env

    check_hash old_summary location src_fn =
        checkSummaryHash
          hsc_env
          (new_summary location (ms_mod $ emsModSummary old_summary) src_fn)
          old_summary location

    find_it = do
        found <- findImportedModule fc fopts units mhome_unit wanted_mod NoPkgQual
        case found of
             Found location mod
                | isJust (ml_hs_file location) ->
                        -- Home package
                         Just <$> just_found location mod

             _ -> return Nothing
                        -- Not found
                        -- (If it is TRULY not found at all, we'll
                        -- error when we actually try to compile)

    just_found location mod = do
                -- Adjust location to point to the hs-boot source file,
                -- hi file, object file, when is_boot says so
        let location' = case is_boot of
              IsBoot -> addBootSuffixLocn location
              NotBoot -> location
            src_fn = expectJust "summarise2" (ml_hs_file location')

                -- Check that it exists
                -- It might have been deleted since the Finder last found it
        maybe_h <- fileHashIfExists src_fn
        case maybe_h of
          Nothing -> return $ Left $ noHsFileErr loc src_fn
          Just h  -> new_summary location' mod src_fn h

    new_summary location mod src_fn src_hash
      = runExceptT $ do
        preimps@PreprocessedImports {..}
            <- getPreprocessedImports hsc_env src_fn Nothing maybe_buf

        -- NB: Despite the fact that is_boot is a top-level parameter, we
        -- don't actually know coming into this function what the HscSource
        -- of the module in question is.  This is because we may be processing
        -- this module because another module in the graph imported it: in this
        -- case, we know if it's a boot or not because of the {-# SOURCE #-}
        -- annotation, but we don't know if it's a signature or a regular
        -- module until we actually look it up on the filesystem.
        let hsc_src
              | is_boot == IsBoot = HsBootFile
              | isHaskellSigFilename src_fn = HsigFile
              | otherwise = HsSrcFile

        when (pi_mod_name /= wanted_mod) $
                throwE $ singleMessage $ mkPlainErrorMsgEnvelope pi_mod_name_loc
                       $ DriverFileModuleNameMismatch pi_mod_name wanted_mod

        let instantiations = fromMaybe [] (homeUnitInstantiations <$> mhome_unit)
        when (hsc_src == HsigFile && isNothing (lookup pi_mod_name instantiations)) $
            throwE $ singleMessage $ mkPlainErrorMsgEnvelope pi_mod_name_loc
                   $ DriverUnexpectedSignature pi_mod_name (checkBuildingCabalPackage dflags) instantiations

        liftIO $ makeNewModSummary hsc_env $ MakeNewModSummary
            { nms_src_fn = src_fn
            , nms_src_hash = src_hash
            , nms_is_boot = is_boot
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
      , nms_is_boot :: IsBootInterface
      , nms_hsc_src :: HscSource
      , nms_location :: ModLocation
      , nms_mod :: Module
      , nms_preimps :: PreprocessedImports
      }

makeNewModSummary :: HscEnv -> MakeNewModSummary -> IO ExtendedModSummary
makeNewModSummary hsc_env MakeNewModSummary{..} = do
  let PreprocessedImports{..} = nms_preimps
  obj_timestamp <- modificationTimeIfExists (ml_obj_file nms_location)
  dyn_obj_timestamp <- modificationTimeIfExists (ml_dyn_obj_file nms_location)
  hi_timestamp <- modificationTimeIfExists (ml_hi_file nms_location)
  hie_timestamp <- modificationTimeIfExists (ml_hie_file nms_location)

  extra_sig_imports <- findExtraSigImports hsc_env nms_hsc_src pi_mod_name
  (implicit_sigs, inst_deps) <- implicitRequirementsShallow hsc_env pi_theimps

  return $ ExtendedModSummary
    { emsModSummary =
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
    , emsInstantiatedUnits = inst_deps
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
  let rn_imps = fmap (first rn_pkg_qual)
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
            MCDiagnostic SevWarning _reason
              -> atomicModifyIORef' warnings $ \i -> (action: i, ())
            MCDiagnostic SevError _reason
              -> atomicModifyIORef' errors   $ \i -> (action: i, ())
            MCFatal
              -> atomicModifyIORef' fatals   $ \i -> (action: i, ())
            _ -> action

        printDeferredDiagnostics = liftIO $
          forM_ [warnings, errors, fatals] $ \ref -> do
            -- This IORef can leak when the dflags leaks, so let us always
            -- reset the content.
            actions <- atomicModifyIORef' ref $ \i -> ([], i)
            sequence_ $ reverse actions

    MC.bracket
      (pushLogHookM (const deferDiagnostics))
      (\_ -> popLogHookM >> printDeferredDiagnostics)
      (\_ -> f)

noModError :: HscEnv -> SrcSpan -> ModuleName -> FindResult -> MsgEnvelope GhcMessage
-- ToDo: we don't have a proper line number for this error
noModError hsc_env loc wanted_mod err
  = mkPlainErrorMsgEnvelope loc $ GhcDriverMessage $ DriverUnknownMessage $ mkPlainError noHints $
    cannotFindModule hsc_env wanted_mod err

noHsFileErr :: SrcSpan -> String -> DriverMessages
noHsFileErr loc path
  = singleMessage $ mkPlainErrorMsgEnvelope loc (DriverFileNotFound path)

moduleNotFoundErr :: ModuleName -> DriverMessages
moduleNotFoundErr mod
  = singleMessage $ mkPlainErrorMsgEnvelope noSrcSpan (DriverModuleNotFound mod)

multiRootsErr :: [ModSummary] -> IO ()
multiRootsErr [] = panic "multiRootsErr"
multiRootsErr summs@(summ1:_)
  = throwOneError $ fmap GhcDriverMessage $
    mkPlainErrorMsgEnvelope noSrcSpan $ DriverDuplicatedModuleDeclaration mod files
  where
    mod = ms_mod summ1
    files = map (expectJust "checkDup" . ml_hs_file . ms_location) summs

cyclicModuleErr :: [ModuleGraphNode] -> SDoc
-- From a strongly connected component we find
-- a single cycle to report
cyclicModuleErr mss
  = assert (not (null mss)) $
    case findCycle graph of
       Nothing   -> text "Unexpected non-cycle" <+> ppr mss
       Just path0 -> vcat
        [ case partitionNodes path0 of
            ([],_) -> text "Module imports form a cycle:"
            (_,[]) -> text "Module instantiations form a cycle:"
            _ -> text "Module imports and instantiations form a cycle:"
        , nest 2 (show_path path0)]
  where
    graph :: [Node NodeKey ModuleGraphNode]
    graph =
      [ DigraphNode
        { node_payload = ms
        , node_key = mkNodeKey ms
        , node_dependencies = get_deps ms
        }
      | ms <- mss
      ]

    get_deps :: ModuleGraphNode -> [NodeKey]
    get_deps = \case
      InstantiationNode iuid ->
        [ NodeKey_Module $ GWIB { gwib_mod = hole, gwib_isBoot = NotBoot }
        | hole <- uniqDSetToList $ instUnitHoles iuid
        ]
      ModuleNode (ExtendedModSummary ms bds) ->
        [ NodeKey_Module $ GWIB { gwib_mod = unLoc m, gwib_isBoot = IsBoot }
        | m <- ms_home_srcimps ms ] ++
        [ NodeKey_Unit inst_unit
        | inst_unit <- bds ] ++
        [ NodeKey_Module $ GWIB { gwib_mod = unLoc m, gwib_isBoot = NotBoot }
        | m <- ms_home_imps    ms ]

    show_path :: [ModuleGraphNode] -> SDoc
    show_path []  = panic "show_path"
    show_path [m] = ppr_node m <+> text "imports itself"
    show_path (m1:m2:ms) = vcat ( nest 6 (ppr_node m1)
                                : nest 6 (text "imports" <+> ppr_node m2)
                                : go ms )
       where
         go []     = [text "which imports" <+> ppr_node m1]
         go (m:ms) = (text "which imports" <+> ppr_node m) : go ms

    ppr_node :: ModuleGraphNode -> SDoc
    ppr_node (ModuleNode m) = text "module" <+> ppr_ms (emsModSummary m)
    ppr_node (InstantiationNode u) = text "instantiated unit" <+> ppr u

    ppr_ms :: ModSummary -> SDoc
    ppr_ms ms = quotes (ppr (moduleName (ms_mod ms))) <+>
                (parens (text (msHsFilePath ms)))


cleanCurrentModuleTempFilesMaybe :: MonadIO m => Logger -> TmpFs -> DynFlags -> m ()
cleanCurrentModuleTempFilesMaybe logger tmpfs dflags =
  unless (gopt Opt_KeepTmpFiles dflags) $
    liftIO $ cleanCurrentModuleTempFiles logger tmpfs


addDepsToHscEnv ::  [HomeModInfo] -> HscEnv -> HscEnv
addDepsToHscEnv deps hsc_env =
  hscUpdateHPT (const $ listHMIToHpt deps) hsc_env

setHPT ::  HomePackageTable -> HscEnv -> HscEnv
setHPT deps hsc_env =
  hscUpdateHPT (const $ deps) hsc_env

-- | Wrap an action to catch and handle exceptions.
wrapAction :: HscEnv -> IO a -> IO (Maybe a)
wrapAction hsc_env k = do
  let lcl_logger = hsc_logger hsc_env
      lcl_dynflags = hsc_dflags hsc_env
  let logg err = printMessages lcl_logger (initDiagOpts lcl_dynflags) (srcErrorMessages err)
  -- MP: It is a bit strange how prettyPrintGhcErrors handles some errors but then we handle
  -- SourceError and ThreadKilled differently directly below. TODO: Refactor to use `catches`
  -- directly. MP should probably use safeTry here to not catch async exceptions but that will regress performance due to
  -- internally using forkIO.
  mres <- MC.try $ liftIO $ prettyPrintGhcErrors lcl_logger $ k
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

withParLog :: TVar LogQueueQueue -> Int -> ((Logger -> Logger) -> RunMakeM b) -> RunMakeM b
withParLog lqq_var k cont = do
  let init_log = liftIO $ do
        -- Make a new log queue
        lq <- newLogQueue k
        -- Add it into the LogQueueQueue
        atomically $ initLogQueue lqq_var lq
        return lq
      finish_log lq = liftIO (finishLogQueue lq)
  MC.bracket init_log finish_log $ \lq -> cont (pushLogHook (const (parLogAction lq)))

withLoggerHsc :: Int -> (HscEnv -> RunMakeM a) -> RunMakeM a
withLoggerHsc k cont  = do
  MakeEnv{withLogger, hsc_env} <- ask
  withLogger k $ \modifyLogger -> do
    let lcl_logger = modifyLogger (hsc_logger hsc_env)
        hsc_env' = hsc_env { hsc_logger = lcl_logger }
    -- Run continuation with modified logger
    cont hsc_env'

-- Executing compilation graph nodes

executeInstantiationNode :: Int
  -> Int
  -> RunMakeM HomePackageTable
  -> InstantiatedUnit
  -> RunMakeM ()
executeInstantiationNode k n wait_deps iu = do
    withLoggerHsc k $ \hsc_env -> do
        -- Wait for the dependencies of this node
        deps <- wait_deps
        -- Output of the logger is mediated by a central worker to
        -- avoid output interleaving
        let lcl_hsc_env = setHPT deps hsc_env
        msg <- asks env_messager
        lift $ MaybeT $ wrapAction lcl_hsc_env $ do
          res <- upsweep_inst lcl_hsc_env msg k n iu
          cleanCurrentModuleTempFilesMaybe (hsc_logger hsc_env) (hsc_tmpfs hsc_env) (hsc_dflags hsc_env)
          return res


executeCompileNode :: Int
  -> Int
  -> Maybe HomeModInfo
  -> RunMakeM HomePackageTable
  -> Maybe [ModuleName] -- List of modules we need to rehydrate before compiling
  -> ModSummary
  -> RunMakeM HomeModInfo
executeCompileNode k n !old_hmi wait_deps mrehydrate_mods mod = do
   MakeEnv{..} <- ask
   deps <- wait_deps
   -- Rehydrate any dependencies if this module had a boot file or is a signature file.
   withLoggerHsc k $ \hsc_env -> do
     hydrated_hsc_env <- liftIO $ maybeRehydrateBefore (setHPT deps hsc_env) mod fixed_mrehydrate_mods
     let -- Use the cached DynFlags which includes OPTIONS_GHC pragmas
         lcl_dynflags = ms_hspp_opts mod
     let lcl_hsc_env =
             -- Localise the hsc_env to use the cached flags
             hscSetFlags lcl_dynflags $
             hydrated_hsc_env
     -- Compile the module, locking with a semphore to avoid too many modules
     -- being compiled at the same time leading to high memory usage.
     lift $ MaybeT (withAbstractSem compile_sem $ wrapAction lcl_hsc_env $ do
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
        _ -> mrehydrate_mods

{- Rehydration, see Note [Rehydrating Modules] -}

rehydrate :: HscEnv        -- ^ The HPT in this HscEnv needs rehydrating.
          -> [HomeModInfo] -- ^ These are the modules we want to rehydrate.
          -> IO HscEnv
rehydrate hsc_env hmis = do
  debugTraceMsg logger 2 $
     text "Re-hydrating loop: "
  new_mods <- fixIO $ \new_mods -> do
      let new_hpt = addListToHpt old_hpt new_mods
      let new_hsc_env = hscUpdateHPT (const new_hpt) hsc_env
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

maybeRehydrateAfter :: HomeModInfo
  -> HscEnv
  -> Maybe [ModuleName]
  -> IO (HomePackageTable, HomeModInfo)
maybeRehydrateAfter hmi new_hsc Nothing = return (hsc_HPT new_hsc, hmi)
maybeRehydrateAfter hmi new_hsc (Just mns) = do
  let new_hpt = hsc_HPT new_hsc
      hmis = map (expectJust "mrAfter" . lookupHpt new_hpt) mns
      new_mod_name = moduleName (mi_module (hm_iface hmi))
  final_hpt <- hsc_HPT <$> rehydrate (new_hsc { hsc_type_env_vars = emptyKnotVars }) (hmi : hmis)
  return (final_hpt, expectJust "rehydrate" $ lookupHpt final_hpt new_mod_name)

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

## Why we need to rehydrate A's ModIface before compiling R.hs

After compiling A.hs we'll have a TypeEnv in which the Id for `f` has a type
type uses the AbstractTyCon T; and a TyCon for `S` that also mentions that same
AbstractTyCon. (Abstract because it came from R.hs-boot; we know nothing about
it.)

When compiling R.hs, we build a TyCon for `T`.  But that TyCon mentions `S`, and
it currently has an AbstractTyCon for `T` inside it.  But we want to build a
fully cyclic structure, in which `S` refers to `T` and `T` refers to `S`.

Solution: **rehydration**.  *Before compiling `R.hs`*, rehydrate all the
ModIfaces below it that depend on R.hs-boot.  To rehydrate a ModIface, call
`rehydrateIface` to convert it to a ModDetails.  It's just a de-serialisation
step, no type inference, just lookups.

Now `S` will be bound to a thunk that, when forced, will "see" the final binding
for `T`; see [Tying the knot](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/tying-the-knot).
But note that this must be done *before* compiling R.hs.

## Why we need to rehydrate A's ModIface after compiling R.hs

When compiling R.hs, the knot-tying stuff above will ensure that `f`'s unfolding
mentions the `LocalId` for `g`.  But when we finish R, we carefully ensure that
all those `LocalIds` are turned into completed `GlobalIds`, replete with
unfoldings etc.   Alas, that will not apply to the occurrences of `g` in `f`'s
unfolding. And if we leave matters like that, they will stay that way, and *all*
subsequent modules that import A will see a crippled unfolding for `f`.

Solution: rehydrate both R and A's ModIface together, right after completing R.hs.

## Which modules to rehydrate

We only need rehydrate modules that are
* Below R.hs
* Above R.hs-boot

There might be many unrelated modules (in the home package) that don't need to be
rehydrated.

## Modules "above" the loop

This dark corner is the subject of #14092.

Suppose we add to our example
```
X.hs     module X where
           import A
           data XT = MkX T
           fx = ...g...
```
If in `--make` we compile R.hs-boot, then A.hs, then X.hs, we'll get a `ModDetails` for `X` that has an AbstractTyCon for `T` in the the argument type of `MkX`.  So:

* Either we should delay compiling X until after R has beeen compiled. (This is what we do)
* Or we should rehydrate X after compiling R -- because it transitively depends on R.hs-boot.

Ticket #20200 has exposed some issues to do with the knot-tying logic in GHC.Make, in `--make` mode.
#20200 has lots of issues, many of them now fixed;
this particular issue starts [here](https://gitlab.haskell.org/ghc/ghc/-/issues/20200#note_385758).

The wiki page [Tying the knot](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/tying-the-knot) is helpful.
Also closely related are
    * #14092
    * #14103

-}


-- | Wait for some dependencies to finish and then read from the given MVar.
wait_deps_hpt :: MVar b -> [ResultVar (Maybe HomeModInfo)] -> ReaderT MakeEnv (MaybeT IO) b
wait_deps_hpt hpt_var deps = do
  _ <- wait_deps deps
  liftIO $ readMVar hpt_var


-- | Wait for dependencies to finish, and then return their results.
wait_deps :: [ResultVar (Maybe HomeModInfo)] -> RunMakeM [HomeModInfo]
wait_deps [] = return []
wait_deps (x:xs) = do
  res <- lift $ waitResult x
  case res of
    Nothing -> wait_deps xs
    Just hmi -> (hmi:) <$> wait_deps xs


-- Executing the pipelines

-- | Start a thread which reads from the LogQueueQueue
logThread :: Logger -> TVar Bool -- Signal that no more new logs will be added, clear the queue and exit
                    -> TVar LogQueueQueue -- Queue for logs
                    -> IO (IO ())
logThread logger stopped lqq_var = do
  finished_var <- newEmptyMVar
  _ <- forkIO $ print_logs *> putMVar finished_var ()
  return (takeMVar finished_var)
  where
    finish = mapM (printLogs logger)

    print_logs = join $ atomically $ do
      lqq <- readTVar lqq_var
      case dequeueLogQueueQueue lqq of
        Just (lq, lqq') -> do
          writeTVar lqq_var lqq'
          return (printLogs logger lq *> print_logs)
        Nothing -> do
          -- No log to print, check if we are finished.
          stopped <- readTVar stopped
          if not stopped then retry
                         else return (finish (allLogQueues lqq))


label_self :: String -> IO ()
label_self thread_name = do
    self_tid <- CC.myThreadId
    CC.labelThread self_tid thread_name


runPipelines :: Int -> HscEnv -> Maybe Messager -> [MakeAction] -> IO ()
runPipelines n_job orig_hsc_env mHscMessager all_pipelines = do
  liftIO $ label_self "main --make thread"

  plugins_hsc_env <- initializePlugins orig_hsc_env Nothing
  case n_job of
    1 -> runSeqPipelines plugins_hsc_env mHscMessager all_pipelines
    _n -> runParPipelines n_job plugins_hsc_env mHscMessager all_pipelines

runSeqPipelines :: HscEnv -> Maybe Messager -> [MakeAction] -> IO ()
runSeqPipelines plugin_hsc_env mHscMessager all_pipelines =
  let env = MakeEnv { hsc_env = plugin_hsc_env
                    , withLogger = \_ k -> k id
                    , compile_sem = AbstractSem (return ()) (return ())
                    , env_messager = mHscMessager
                    }
  in runAllPipelines 1 env all_pipelines


-- | Build and run a pipeline
runParPipelines :: Int              -- ^ How many capabilities to use
             -> HscEnv           -- ^ The basic HscEnv which is augmented with specific info for each module
             -> Maybe Messager   -- ^ Optional custom messager to use to report progress
             -> [MakeAction]  -- ^ The build plan for all the module nodes
             -> IO ()
runParPipelines n_jobs plugin_hsc_env mHscMessager all_pipelines = do


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

  let updNumCapabilities = liftIO $ do
          n_capabilities <- getNumCapabilities
          n_cpus <- getNumProcessors
          -- Setting number of capabilities more than
          -- CPU count usually leads to high userspace
          -- lock contention. #9221
          let n_caps = min n_jobs n_cpus
          unless (n_capabilities /= 1) $ setNumCapabilities n_caps
          return n_capabilities

  let resetNumCapabilities orig_n = do
          liftIO $ setNumCapabilities orig_n
          atomically $ writeTVar stopped_var True
          wait_log_thread

  compile_sem <- newQSem n_jobs
  let abstract_sem = AbstractSem (waitQSem compile_sem) (signalQSem compile_sem)
    -- Reset the number of capabilities once the upsweep ends.
  let env = MakeEnv { hsc_env = thread_safe_hsc_env
                    , withLogger = withParLog log_queue_queue_var
                    , compile_sem = abstract_sem
                    , env_messager = mHscMessager
                    }

  MC.bracket updNumCapabilities resetNumCapabilities $ \_ ->
    runAllPipelines n_jobs env all_pipelines

withLocalTmpFS :: RunMakeM a -> RunMakeM a
withLocalTmpFS act = do
  let initialiser = do
        MakeEnv{..} <- ask
        lcl_tmpfs <- liftIO $ forkTmpFsFrom (hsc_tmpfs hsc_env)
        return $ hsc_env { hsc_tmpfs  = lcl_tmpfs }
      finaliser lcl_env = do
        gbl_env <- ask
        liftIO $ mergeTmpFsInto (hsc_tmpfs lcl_env) (hsc_tmpfs (hsc_env gbl_env))
       -- Add remaining files which weren't cleaned up into local tmp fs for
       -- clean-up later.
       -- Clear the logQueue if this node had it's own log queue
  MC.bracket initialiser finaliser $ \lcl_hsc_env -> local (\env -> env { hsc_env = lcl_hsc_env}) act

-- | Run the given actions and then wait for them all to finish.
runAllPipelines :: Int -> MakeEnv -> [MakeAction] -> IO ()
runAllPipelines n_jobs env acts = do
  let spawn_actions :: IO [ThreadId]
      spawn_actions = if n_jobs == 1
        then (:[]) <$> (forkIOWithUnmask $ \unmask -> void $ runLoop (\io -> io unmask) env acts)
        else runLoop forkIOWithUnmask env acts

      kill_actions :: [ThreadId] -> IO ()
      kill_actions tids = mapM_ killThread tids

  MC.bracket spawn_actions kill_actions $ \_ -> do
    mapM_ waitMakeAction acts

-- | Execute each action in order, limiting the amount of parrelism by the given
-- semaphore.
runLoop :: (((forall a. IO a -> IO a) -> IO ()) -> IO a) -> MakeEnv -> [MakeAction] -> IO [a]
runLoop _ _env [] = return []
runLoop fork_thread env (MakeAction act res_var :acts) = do
  new_thread <-
    fork_thread $ \unmask -> (do
            mres <- (unmask $ run_pipeline (withLocalTmpFS act))
                      `MC.onException` (putMVar res_var Nothing) -- Defensive: If there's an unhandled exception then still signal the failure.
            putMVar res_var mres)
  threads <- runLoop fork_thread env acts
  return (new_thread : threads)
  where
      run_pipeline :: RunMakeM a -> IO (Maybe a)
      run_pipeline p = runMaybeT (runReaderT p env)

data MakeAction = forall a . MakeAction (RunMakeM a) (MVar (Maybe a))

waitMakeAction :: MakeAction -> IO ()
waitMakeAction (MakeAction _ mvar) = () <$ readMVar mvar

{- Note [GHC Heap Invariants]

This note is a general place to explain some of the heap invariants which should
hold for a program compiled with --make mode. These invariants are all things
which can be checked easily using ghc-debug.

1. No HomeModInfo are reachable via the EPS.
   Why? Interfaces are lazily loaded into the EPS and the lazy thunk retains
        a reference to the entire HscEnv, if we are not careful the HscEnv will
        contain the HomePackageTable at the time the interface was loaded and
        it will never be released.
   Where? dontLeakTheHPT in GHC.Iface.Load

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

-}
