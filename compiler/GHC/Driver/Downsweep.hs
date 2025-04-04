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
module GHC.Driver.Downsweep
  ( downsweep
  , downsweepThunk
  , downsweepInstalledModules
  , downsweepFromRootNodes
  , DownsweepMode(..)
   -- * Summary functions
  , summariseModule
  , summariseFile
  , summariseModuleInterface
  , SummariseResult(..)
  -- * Helper functions
  , instantiationNodes
  , checkHomeUnitsClosed
  ) where

import GHC.Prelude

import GHC.Platform.Ways

import GHC.Driver.Config.Finder (initFinderOpts)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Phases
import {-# SOURCE #-} GHC.Driver.Pipeline (preprocess)
import GHC.Driver.Session
import GHC.Driver.Backend
import GHC.Driver.Monad
import GHC.Driver.Env
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Messager
import GHC.Driver.MakeSem
import GHC.Driver.MakeAction
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Ppr

import GHC.Iface.Load

import GHC.Parser.Header
import GHC.Rename.Names
import GHC.Tc.Utils.Backpack

import GHC.Data.Graph.Directed
import GHC.Data.FastString
import GHC.Data.Maybe      ( expectJust )
import qualified GHC.Data.Maybe as M
import GHC.Data.OsPath     ( unsafeEncodeUtf )
import GHC.Data.StringBuffer
import GHC.Data.Graph.Directed.Reachability
import qualified GHC.LanguageExtensions as LangExt

import GHC.Utils.Exception ( throwIO, SomeAsyncException )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Error
import GHC.Utils.Logger
import GHC.Utils.Fingerprint
import GHC.Utils.TmpFs
import GHC.Utils.Constants

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
import GHC.Unit.Module.Deps
import qualified GHC.Unit.Home.Graph as HUG

import Data.Either ( rights, partitionEithers, lefts )
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans.Except ( ExceptT(..), runExceptT, throwE )
import qualified Control.Monad.Catch as MC
import Data.Maybe
import Data.List (partition)
import Data.Time
import Data.List (unfoldr)
import Data.Bifunctor (first)
import System.Directory
import System.FilePath

import Control.Monad.Trans.Reader
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Class
import System.IO.Unsafe (unsafeInterleaveIO)

{-
Note [Downsweep and the ModuleGraph]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ModuleGraph stores the relationship between all the modules, units, and
instantiations in the current session.

When we do downsweep, we build up a new ModuleGraph, starting from the root
modules. By following all the dependencies we construct a graph which allows
us to answer questions about the transitive closure of the imports.

The module graph is accessible in the HscEnv.

When is this graph constructed?

1. In `--make` mode, we construct the graph before starting to do any compilation.

2. In `-c` (oneshot) mode, we construct the graph when we have calculated the
   ModSummary for the module we are compiling. The `ModuleGraph` is stored in a
   thunk, so it is only constructed when it is needed. This avoids reading
   the interface files of the whole transitive closure unless they are needed.

3. In some situations (such as loading plugins) we may need to construct the
   graph without having a ModSummary. In this case we use the `downsweepInstalledModules`
   function.

The result is having a uniform graph available for the whole compilation pipeline.

-}

-- This caches the answer to the question, if we are in this unit, what does
-- an import of this module mean.
type DownsweepCache = M.Map (UnitId, PkgQual, ModuleNameWithIsBoot) [Either DriverMessages ModuleNodeInfo]

-----------------------------------------------------------------------------
--
-- | Downsweep (dependency analysis) for --make mode
--
-- Chase downwards from the specified root set, returning summaries
-- for all home modules encountered.  Only follow source-import
-- links.
--
-- We pass in the previous collection of summaries, which is used as a
-- cache to avoid recalculating a module summary if the source is
-- unchanged.
--
-- The returned ModuleGraph has one node for each home-package
-- module, plus one for any hs-boot files.  The imports of these nodes
-- are all there, including the imports of non-home-package modules.
--
-- This function is intendned for use by --make mode and will also insert
-- LinkNodes and InstantiationNodes for any home units.
--
-- It will also turn on code generation for any modules that need it by calling
-- 'enableCodeGenForTH'.
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
          -> IO ([DriverMessages], ModuleGraph)
                -- The non-error elements of the returned list all have distinct
                -- (Modules, IsBoot) identifiers, unless the Bool is true in
                -- which case there can be repeats
downsweep hsc_env diag_wrapper msg old_summaries excl_mods allow_dup_roots = do
  n_jobs <- mkWorkerLimit (hsc_dflags hsc_env)
  (root_errs, root_summaries) <- rootSummariesParallel n_jobs hsc_env diag_wrapper msg summary
  let closure_errs = checkHomeUnitsClosed unit_env
      unit_env = hsc_unit_env hsc_env

      all_errs = closure_errs ++ root_errs

  case all_errs of
    [] -> do
       (downsweep_errs, downsweep_nodes) <- downsweepFromRootNodes hsc_env old_summary_map excl_mods allow_dup_roots DownsweepUseCompile (map ModuleNodeCompile root_summaries) []

       let (other_errs, unit_nodes) = partitionEithers $ HUG.unitEnv_foldWithKey (\nodes uid hue -> nodes ++ unitModuleNodes downsweep_nodes uid hue) [] (hsc_HUG hsc_env)

       let all_nodes = downsweep_nodes ++ unit_nodes
       let all_errs = downsweep_errs ++ other_errs

       let logger = hsc_logger hsc_env
           tmpfs = hsc_tmpfs hsc_env
       -- if we have been passed -fno-code, we enable code generation
       -- for dependencies of modules that have -XTemplateHaskell,
       -- otherwise those modules will fail to compile.
       -- See Note [-fno-code mode] #8025
       th_configured_nodes <- enableCodeGenForTH logger tmpfs unit_env all_nodes

       return (all_errs, th_configured_nodes)
    _  -> return (all_errs, emptyMG)
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

    -- Dependencies arising on a unit (backpack and module linking deps)
    unitModuleNodes :: [ModuleGraphNode] -> UnitId -> HomeUnitEnv -> [Either (Messages DriverMessage) ModuleGraphNode]
    unitModuleNodes summaries uid hue =
      maybeToList (linkNodes summaries uid hue)

-- | Calculate the module graph starting from a single ModSummary. The result is a
-- thunk, which when forced will perform the downsweep. This is useful in oneshot
-- mode where the module graph may never be needed.
-- If downsweep fails, then the resulting errors are just thrown.
downsweepThunk :: HscEnv -> ModSummary -> IO ModuleGraph
downsweepThunk hsc_env mod_summary = unsafeInterleaveIO $ do
  debugTraceMsg (hsc_logger hsc_env) 3 $ text "Computing Module Graph thunk..."
  ~(errs, mg) <- downsweepFromRootNodes hsc_env mempty [] True DownsweepUseFixed [ModuleNodeCompile mod_summary] []
  let dflags = hsc_dflags hsc_env
  liftIO $ printOrThrowDiagnostics (hsc_logger hsc_env)
                                   (initPrintConfig dflags)
                                   (initDiagOpts dflags)
                                   (GhcDriverMessage <$> unionManyMessages errs)
  return (mkModuleGraph mg)

-- | Create a module graph from a list of installed modules.
-- This is used by the loader when we need to load modules but there
-- isn't already an existing module graph. For example, when loading plugins
-- during initialisation.
--
-- If you call this function, then if the `Module` you request to downsweep can't
-- be found then this function will throw errors.
-- If you need to use this function elsewhere, then it would make sense to make it
-- return [DriverMessages] and [ModuleGraph] so that the caller can handle the errors as it sees fit.
-- At the moment, it is overfitted for what `get_reachable_nodes` needs.
downsweepInstalledModules :: HscEnv -> [Module] -> IO ModuleGraph
downsweepInstalledModules hsc_env mods = do
    let
        (home_mods, external_mods) = partition (\u -> moduleUnitId u `elem` hsc_all_home_unit_ids hsc_env) mods
        installed_mods = map (fst . getModuleInstantiation) home_mods
        external_uids = map moduleUnitId external_mods

        process :: InstalledModule -> IO ModuleNodeInfo
        process i = do
          res <- findExactModule hsc_env i NotBoot
          case res of
            InstalledFound loc -> return $ ModuleNodeFixed (installedModuleToMnk i) loc
            -- It is an internal-ish error if this happens, since we any call to this function should
            -- already know that we can find the modules we need to load.
            _ -> throwGhcException $ ProgramError $ showSDoc (hsc_dflags hsc_env) $ text "downsweepInstalledModules: Could not find installed module" <+> ppr i

    nodes <- mapM process installed_mods
    (errs, mg) <- downsweepFromRootNodes hsc_env mempty [] True DownsweepUseFixed nodes external_uids

    -- Similarly here, we should really not get any errors, but print them out if we do.
    let dflags = hsc_dflags hsc_env
    liftIO $ printOrThrowDiagnostics (hsc_logger hsc_env)
                                     (initPrintConfig dflags)
                                     (initDiagOpts dflags)
                                     (GhcDriverMessage <$> unionManyMessages errs)

    return (mkModuleGraph mg)



-- | Whether downsweep should use compiler or fixed nodes. Compile nodes are used
-- by --make mode, and fixed nodes by oneshot mode.
--
-- See Note [Module Types in the ModuleGraph] for the difference between the two.
data DownsweepMode = DownsweepUseCompile | DownsweepUseFixed

-- | Perform downsweep, starting from the given root 'ModuleNodeInfo's and root
-- 'UnitId's.
-- This function will start at the given roots, and traverse downwards to find
-- all the dependencies, all the way to the leaf units.
downsweepFromRootNodes :: HscEnv
                  -> M.Map (UnitId, FilePath) ModSummary
                  -> [ModuleName]
                  -> Bool
                  -> DownsweepMode -- ^ Whether to create fixed or compile nodes for dependencies
                  -> [ModuleNodeInfo] -- ^ The starting ModuleNodeInfo
                  -> [UnitId] -- ^ The starting units
                  -> IO ([DriverMessages], [ModuleGraphNode])
downsweepFromRootNodes hsc_env old_summaries excl_mods allow_dup_roots mode root_nodes root_uids
   = do
       let root_map = mkRootMap root_nodes
       checkDuplicates root_map
       (module_deps, map0) <- loopModuleNodeInfos root_nodes (M.empty, root_map)
       let all_deps = loopUnit hsc_env module_deps root_uids

       let all_instantiations =  getHomeUnitInstantiations hsc_env
       let deps' = loopInstantiations all_instantiations all_deps

           downsweep_errs = lefts $ concat $ M.elems map0
           downsweep_nodes = M.elems deps'

       return (downsweep_errs, downsweep_nodes)
     where
        getHomeUnitInstantiations :: HscEnv -> [(UnitId, InstantiatedUnit)]
        getHomeUnitInstantiations hsc_env = HUG.unitEnv_foldWithKey (\nodes uid hue -> nodes ++  instantiationNodes uid (homeUnitEnv_units hue)) [] (hsc_HUG hsc_env)


        calcDeps ms =
          -- Add a dependency on the HsBoot file if it exists
          -- This gets passed to the loopImports function which just ignores it if it
          -- can't be found.
          [(ms_unitid ms, NoPkgQual, GWIB (noLoc $ ms_mod_name ms) IsBoot) | NotBoot <- [isBootSummary ms] ] ++
          [(ms_unitid ms, b, c) | (b, c) <- msDeps ms ]

        -- In a root module, the filename is allowed to diverge from the module
        -- name, so we have to check that there aren't multiple root files
        -- defining the same module (otherwise the duplicates will be silently
        -- ignored, leading to confusing behaviour).
        checkDuplicates
          :: DownsweepCache
          -> IO ()
        checkDuplicates root_map
           | not allow_dup_roots
           , dup_root:_ <- dup_roots = liftIO $ multiRootsErr dup_root
           | otherwise = pure ()
           where
             dup_roots :: [[ModuleNodeInfo]]        -- Each at least of length 2
             dup_roots = filterOut isSingleton $ map rights (M.elems root_map)

        loopInstantiations :: [(UnitId, InstantiatedUnit)]
                           -> M.Map NodeKey ModuleGraphNode
                           -> M.Map NodeKey ModuleGraphNode
        loopInstantiations [] done = done
        loopInstantiations ((home_uid, iud) :xs) done =
          let hsc_env' = hscSetActiveHomeUnit home_unit hsc_env
              done' = loopUnit hsc_env' done [instUnitInstanceOf iud]
              payload = InstantiationNode home_uid iud
          in loopInstantiations xs (M.insert (mkNodeKey payload) payload done')

          where
            home_unit = ue_unitHomeUnit home_uid (hsc_unit_env hsc_env)


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
             loopSummaries next (M.insert k (ModuleNode final_deps (ModuleNodeCompile ms)) done'', summarised'')
          where
            k = NodeKey_Module (msKey ms)

            hs_file_for_boot
              | HsBootFile <- ms_hsc_src ms
              = Just $ ((ms_unitid ms), NoPkgQual, (GWIB (noLoc $ ms_mod_name ms) NotBoot))
              | otherwise
              = Nothing

        loopModuleNodeInfos :: [ModuleNodeInfo] -> (M.Map NodeKey ModuleGraphNode, DownsweepCache) -> IO (M.Map NodeKey ModuleGraphNode, DownsweepCache)
        loopModuleNodeInfos is cache = foldM (flip loopModuleNodeInfo) cache is

        loopModuleNodeInfo :: ModuleNodeInfo -> (M.Map NodeKey ModuleGraphNode, DownsweepCache) -> IO (M.Map NodeKey ModuleGraphNode, DownsweepCache)
        loopModuleNodeInfo mod_node_info (done, summarised) = do
          case mod_node_info of
            ModuleNodeCompile ms -> do
              loopSummaries [ms] (done, summarised)
            ModuleNodeFixed mod ml -> do
              done' <- loopFixedModule mod ml done
              return (done', summarised)

        -- NB: loopFixedModule does not take a downsweep cache, because if you
        -- ever reach a Fixed node, everything under that also must be fixed.
        loopFixedModule :: ModNodeKeyWithUid -> ModLocation
                        -> M.Map NodeKey ModuleGraphNode
                        -> IO (M.Map NodeKey ModuleGraphNode)
        loopFixedModule key loc done = do
          let nk = NodeKey_Module key
          case M.lookup nk done of
            Just {} -> return done
            Nothing -> do
              -- MP: TODO, we should just read the dependency info from the interface rather than either
              -- a. Loading the whole thing into the EPS (this might never nececssary and causes lots of things to be permanently loaded into memory)
              -- b. Loading the whole interface into a buffer before discarding it. (wasted allocation and deserialisation)
              read_result <-
                -- 1. Check if the interface is already loaded into the EPS by some other
                -- part of the compiler.
                lookupIfaceByModuleHsc hsc_env (mnkToModule key) >>= \case
                  Just iface -> return (M.Succeeded iface)
                  Nothing -> readIface (hsc_logger hsc_env) (hsc_dflags hsc_env) (hsc_NC hsc_env) (mnkToModule key) (ml_hi_file loc)
              case read_result of
                M.Succeeded iface -> do
                  -- Computer information about this node
                  let node_deps = ifaceDeps (mi_deps iface)
                      edges = map (either NodeKey_Module NodeKey_ExternalUnit) node_deps
                      node = ModuleNode edges (ModuleNodeFixed key loc)
                  foldM (loopFixedNodeKey (mnkUnitId key)) (M.insert nk node done) node_deps
                -- Ignore any failure, we might try to read a .hi-boot file for
                -- example, even if there is not one.
                M.Failed {} ->
                  return done

        loopFixedNodeKey :: UnitId -> M.Map NodeKey ModuleGraphNode -> Either ModNodeKeyWithUid UnitId -> IO (M.Map NodeKey ModuleGraphNode)
        loopFixedNodeKey _ done (Left key) = do
          loopFixedImports [key] done
        loopFixedNodeKey home_uid done (Right uid) = do
          -- Set active unit so that looking loopUnit finds the correct
          -- -package flags in the unit state.
          let hsc_env' = hscSetActiveUnitId home_uid hsc_env
          return $ loopUnit hsc_env' done [uid]


        ifaceDeps :: Dependencies -> [Either ModNodeKeyWithUid UnitId]
        ifaceDeps deps =
          [ Left (ModNodeKeyWithUid dep uid)
          | (uid, dep) <- Set.toList (dep_direct_mods deps)
          ] ++
          [ Right uid
          | uid <- Set.toList (dep_direct_pkgs deps)
          ]

        -- Like loopImports, but we already know exactly which module we are looking for.
        loopFixedImports :: [ModNodeKeyWithUid]
                         -> M.Map NodeKey ModuleGraphNode
                         -> IO (M.Map NodeKey ModuleGraphNode)
        loopFixedImports [] done = pure done
        loopFixedImports (key:keys) done = do
          let nk = NodeKey_Module key
          case M.lookup nk done of
            Just {} -> loopFixedImports keys done
            Nothing -> do
              read_result <- findExactModule hsc_env (mnkToInstalledModule key) (mnkIsBoot key)
              case read_result of
                InstalledFound loc -> do
                  done' <- loopFixedModule key loc done
                  loopFixedImports keys done'
                _otherwise ->
                  -- If the finder fails, just keep going, there will be another
                  -- error later.
                  loopFixedImports keys done

        downsweepSummarise :: HscEnv
                           -> HomeUnit
                           -> M.Map (UnitId, FilePath) ModSummary
                           -> IsBootInterface
                           -> Located ModuleName
                           -> PkgQual
                           -> Maybe (StringBuffer, UTCTime)
                           -> [ModuleName]
                           -> IO SummariseResult
        downsweepSummarise hsc_env home_unit old_summaries is_boot wanted_mod mb_pkg maybe_buf excl_mods =
          case mode of
            DownsweepUseCompile -> summariseModule hsc_env home_unit old_summaries is_boot wanted_mod mb_pkg maybe_buf excl_mods
            DownsweepUseFixed -> summariseModuleInterface hsc_env home_unit is_boot wanted_mod mb_pkg excl_mods


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
                let nk = NodeKey_Module (mnKey ms)
                (rest, summarised', done') <- loopImports ss done summarised
                return (nk: rest, summarised', done')
              [Left _err] ->
                loopImports ss done summarised
              _errs ->  do
                loopImports ss done summarised
          | otherwise
          = do
               mb_s <- downsweepSummarise hsc_env home_unit old_summaries
                                       is_boot wanted_mod mb_pkg
                                       Nothing excl_mods
               case mb_s of
                   NotThere -> loopImports ss done summarised
                   External uid -> do
                    -- Pass an updated hsc_env to loopUnit, as each unit might
                    -- have a different visible package database.
                    let hsc_env' = hscSetActiveHomeUnit home_unit hsc_env
                    let done' = loopUnit hsc_env' done [uid]
                    (other_deps, done'', summarised') <- loopImports ss done' summarised
                    return (NodeKey_ExternalUnit uid : other_deps, done'', summarised')
                   FoundInstantiation iud -> do
                    (other_deps, done', summarised') <- loopImports ss done summarised
                    return (NodeKey_Unit iud : other_deps, done', summarised')
                   FoundHomeWithError (_uid, e) ->  loopImports ss done (Map.insert cache_key [(Left e)] summarised)
                   FoundHome s -> do
                     (done', summarised') <-
                       loopModuleNodeInfo s (done, Map.insert cache_key [Right s] summarised)
                     (other_deps, final_done, final_summarised) <- loopImports ss done' summarised'

                     -- MP: This assumes that we can only instantiate non home units, which is probably fair enough for now.
                     return (NodeKey_Module (mnKey s) : other_deps, final_done, final_summarised)
          where
            cache_key = (home_uid, mb_pkg, unLoc <$> gwib)
            home_unit = ue_unitHomeUnit home_uid (hsc_unit_env hsc_env)
            GWIB { gwib_mod = L loc mod, gwib_isBoot = is_boot } = gwib
            wanted_mod = L loc mod

        loopUnit :: HscEnv -> Map.Map NodeKey ModuleGraphNode -> [UnitId] -> Map.Map NodeKey ModuleGraphNode
        loopUnit _ cache [] = cache
        loopUnit lcl_hsc_env cache (u:uxs) = do
           let nk = (NodeKey_ExternalUnit u)
           case Map.lookup nk cache of
             Just {} -> loopUnit lcl_hsc_env cache uxs
             Nothing -> case unitDepends <$> lookupUnitId (hsc_units lcl_hsc_env) u of
                         Just us -> loopUnit lcl_hsc_env (loopUnit lcl_hsc_env (Map.insert nk (UnitNode us u) cache) us) uxs
                         Nothing -> pprPanic "loopUnit" (text "Malformed package database, missing " <+> ppr u)

multiRootsErr :: [ModuleNodeInfo] -> IO ()
multiRootsErr [] = panic "multiRootsErr"
multiRootsErr summs@(summ1:_)
  = throwOneError $ fmap GhcDriverMessage $
    mkPlainErrorMsgEnvelope noSrcSpan $ DriverDuplicatedModuleDeclaration mod files
  where
    mod = moduleNodeInfoModule summ1
    files = mapMaybe (ml_hs_file . moduleNodeInfoLocation) summs

moduleNotFoundErr :: UnitId -> ModuleName -> DriverMessages
moduleNotFoundErr uid mod = singleMessage $ mkPlainErrorMsgEnvelope noSrcSpan (DriverModuleNotFound uid mod)

-- | Collect the instantiations of dependencies to create 'InstantiationNode' work graph nodes.
-- These are used to represent the type checking that is done after
-- all the free holes (sigs in current package) relevant to that instantiation
-- are compiled. This is necessary to catch some instantiation errors.
instantiationNodes :: UnitId -> UnitState -> [(UnitId, InstantiatedUnit)]
instantiationNodes uid unit_state = map (uid,) iuids_to_check
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
      unit_nodes = map mkNodeKey (filter ((== uid) . mgNodeUnitId) summaries)
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

getRootSummary ::
  [ModuleName] ->
  M.Map (UnitId, FilePath) ModSummary ->
  HscEnv ->
  Target ->
  IO (Either DriverMessages ModSummary)
getRootSummary excl_mods old_summary_map hsc_env target
  | TargetFile file mb_phase <- targetId
  = do
    let offset_file = augmentByWorkingDirectory dflags file
    exists <- liftIO $ doesFileExist offset_file
    if exists || isJust maybe_buf
    then summariseFile hsc_env home_unit old_summary_map offset_file mb_phase
         maybe_buf
    else
      return $ Left $ singleMessage $
      mkPlainErrorMsgEnvelope noSrcSpan (DriverFileNotFound offset_file)
  | TargetModule modl <- targetId
  = do
    maybe_summary <- summariseModule hsc_env home_unit old_summary_map NotBoot
                     (L rootLoc modl) (ThisPkg (homeUnitId home_unit))
                     maybe_buf excl_mods
    pure case maybe_summary of
      FoundHome (ModuleNodeCompile s)  -> Right s
      FoundHomeWithError err -> Left (snd err)
      _ -> Left (moduleNotFoundErr uid modl)
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
  (HscEnv -> Target -> IO (Either DriverMessages ModSummary)) ->
  IO ([DriverMessages], [ModSummary])
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
    home_id_set = HUG.allUnits $ ue_home_unit_graph ue
    bad_unit_ids = upwards_closure Set.\\ home_id_set {- Remove all home units reached, keep only bad nodes -}
    rootLoc = mkGeneralSrcSpan (fsLit "<command line>")

    downwards_closure :: Graph (Node UnitId UnitId)
    downwards_closure = graphFromEdgedVerticesUniq graphNodes

    inverse_closure = graphReachability $ transposeG downwards_closure

    upwards_closure = Set.fromList $ map node_key $ allReachableMany inverse_closure [DigraphNode uid uid [] | uid <- Set.toList home_id_set]

    all_unit_direct_deps :: UniqMap UnitId (Set.Set UnitId)
    all_unit_direct_deps
      = HUG.unitEnv_foldWithKey go emptyUniqMap $ ue_home_unit_graph ue
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
  -> IO ModuleGraph
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
  -> IO ModuleGraph
enableCodeGenWhen logger tmpfs staticLife dynLife unit_env mod_graph = do
  mgMapM enable_code_gen mg
  where
    defaultBackendOf ms = platformDefaultBackend (targetPlatform $ ue_unitFlags (ms_unitid ms) unit_env)

    enable_code_gen :: ModuleNodeInfo -> IO ModuleNodeInfo
    enable_code_gen (ModuleNodeCompile ms) = ModuleNodeCompile <$> enable_code_gen_ms ms
    enable_code_gen m@(ModuleNodeFixed {}) = return m

    -- FIXME: Strong resemblance and some duplication between this and `makeDynFlagsConsistent`.
    -- It would be good to consider how to make these checks more uniform and not duplicated.
    enable_code_gen_ms :: ModSummary -> IO ModSummary
    enable_code_gen_ms ms
      | ModSummary
        { ms_location = ms_location
        , ms_hsc_src = HsSrcFile
        , ms_hspp_opts = dflags
        } <- ms
      , Just enable_spec <- needs_codegen_map (NodeKey_Module (msKey ms)) =
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
               enable_code_gen_ms ms'

         -- If -fprefer-byte-code then satisfy dependency by enabling bytecode (if normal object not enough)
         -- we only get to this case if the default backend is already generating object files, but we need dynamic
         -- objects
         | bytecode_and_enable enable_spec ms -> do
               let ms' = ms
                     { ms_hspp_opts = gopt_set (ms_hspp_opts ms) Opt_ByteCodeAndObjectCode
                     }
               -- Recursive call to catch the other cases
               enable_code_gen_ms ms'
         | dynamic_too_enable enable_spec ms -> do
               let ms' = ms
                     { ms_hspp_opts = gopt_set (ms_hspp_opts ms) Opt_BuildDynamicToo
                     }
               -- Recursive call to catch the other cases
               enable_code_gen_ms ms'
         | ext_interp_enable ms -> do
               let ms' = ms
                     { ms_hspp_opts = gopt_set (ms_hspp_opts ms) Opt_ExternalInterpreter
                     }
               -- Recursive call to catch the other cases
               enable_code_gen_ms ms'

         | needs_full_ways dflags -> do
               let ms' = ms { ms_hspp_opts = set_full_ways dflags }
               -- Recursive call to catch the other cases
               enable_code_gen_ms ms'

         | otherwise -> return ms

    enable_code_gen_ms ms = return ms

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

    mg = mkModuleGraph mod_graph

    needs_obj_set, needs_bc_set :: NodeKey -> Bool
    needs_obj_set k = mgQueryMany mg need_obj_set k || k `elem` need_obj_set

    needs_bc_set k = mgQueryMany mg need_bc_set k || k `elem` need_bc_set

    -- A map which tells us how to enable code generation for a NodeKey
    needs_codegen_map :: NodeKey -> Maybe CodeGenEnable
    needs_codegen_map nk =
      -- Another option here would be to just produce object code, rather than both object and
      -- byte code
      case (needs_obj_set nk, needs_bc_set nk) of
        (True, True)   -> Just EnableByteCodeAndObject
        (True, False)  -> Just EnableObject
        (False, True)  -> Just EnableByteCode
        (False, False) -> Nothing

    -- The direct dependencies of modules which require object code
    need_obj_set =
      concat
        -- Note we don't need object code for a module if it uses TemplateHaskell itself. Only
        -- it's dependencies.
        [ deps
        | (ModuleNode deps (ModuleNodeCompile ms)) <- mod_graph
        , isTemplateHaskellOrQQNonBoot ms
        , not (gopt Opt_UseBytecodeRatherThanObjects (ms_hspp_opts ms))
        ]

    -- The direct dependencies of modules which require byte code
    need_bc_set =
      concat
        [ deps
        | (ModuleNode deps (ModuleNodeCompile ms)) <- mod_graph
        , isTemplateHaskellOrQQNonBoot ms
        , gopt Opt_UseBytecodeRatherThanObjects (ms_hspp_opts ms)
        ]

    -- FIXME: Duplicated from makeDynFlagsConsistent
    needs_full_ways dflags
      = ghcLink dflags == LinkInMemory &&
        not (gopt Opt_ExternalInterpreter dflags) &&
        targetWays_ dflags /= hostFullWays
    set_full_ways dflags =
        let platform = targetPlatform dflags
            dflags_a = dflags { targetWays_ = hostFullWays }
            dflags_b = foldl gopt_set dflags_a
                     $ concatMap (wayGeneralFlags platform)
                                 hostFullWays
            dflags_c = foldl gopt_unset dflags_b
                     $ concatMap (wayUnsetGeneralFlags platform)
                                 hostFullWays
        in dflags_c

-- | Populate the Downsweep cache with the root modules.
mkRootMap
  :: [ModuleNodeInfo]
  -> DownsweepCache
mkRootMap summaries = Map.fromListWith (flip (++))
  [ ((moduleNodeInfoUnitId s, NoPkgQual, moduleNodeInfoMnwib s), [Right s]) | s <- summaries ]

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
      | FoundHome ModuleNodeInfo
      | External UnitId
      | NotThere

-- | summariseModule finds the location of the source file for the given module.
-- This version always returns a ModuleNodeCompile node, it is useful for
-- --make mode.
summariseModule :: HscEnv
                -> HomeUnit
                -> M.Map (UnitId, FilePath) ModSummary
                -> IsBootInterface
                -> Located ModuleName
                -> PkgQual
                -> Maybe (StringBuffer, UTCTime)
                -> [ModuleName]
                -> IO SummariseResult
summariseModule hsc_env home_unit old_summaries is_boot wanted_mod mb_pkg maybe_buf excl_mods =
  summariseModuleDispatch k hsc_env home_unit is_boot wanted_mod mb_pkg excl_mods
  where
    k = summariseModuleWithSource home_unit old_summaries is_boot maybe_buf


-- | Like summariseModule but for interface files that we don't want to compile.
-- This version always returns a ModuleNodeFixed node.
summariseModuleInterface :: HscEnv
                        -> HomeUnit
                        -> IsBootInterface
                        -> Located ModuleName
                        -> PkgQual
                        -> [ModuleName]
                        -> IO SummariseResult
summariseModuleInterface hsc_env home_unit is_boot wanted_mod mb_pkg excl_mods =
  summariseModuleDispatch k hsc_env home_unit is_boot wanted_mod mb_pkg excl_mods
  where
    k _hsc_env loc mod = do
      -- The finder will return a path to the .hi-boot even if it doesn't actually
      -- exist. So check if it exists first before concluding it's there.
      does_exist <- doesFileExist (ml_hi_file loc)
      if does_exist
        then let key = moduleToMnk mod is_boot
             in return $ FoundHome (ModuleNodeFixed key loc)
        else return NotThere



-- Summarise a module, and pick up source and timestamp.
summariseModuleDispatch
          :: (HscEnv -> ModLocation -> Module -> IO SummariseResult) -- ^ Continuation about how to summarise a home module.
          -> HscEnv
          -> HomeUnit
          -> IsBootInterface    -- True <=> a {-# SOURCE #-} import
          -> Located ModuleName -- Imported module to be summarised
          -> PkgQual
          -> [ModuleName]               -- Modules to exclude
          -> IO SummariseResult


summariseModuleDispatch k hsc_env' home_unit is_boot (L _ wanted_mod) mb_pkg excl_mods
  | wanted_mod `elem` excl_mods
  = return NotThere
  | otherwise  = find_it
  where
    -- Temporarily change the currently active home unit so all operations
    -- happen relative to it
    hsc_env   = hscSetActiveHomeUnit home_unit hsc_env'

    find_it :: IO SummariseResult

    find_it = do
        found <- findImportedModuleWithIsBoot hsc_env wanted_mod is_boot mb_pkg
        case found of
             Found location mod
                | moduleUnitId mod `Set.member` hsc_all_home_unit_ids hsc_env ->
                        -- Home package
                         k hsc_env location mod
                | VirtUnit iud <- moduleUnit mod
                , not (isHomeModule home_unit mod)
                  -> return $ FoundInstantiation iud
                | otherwise -> return $ External (moduleUnitId mod)
             _ -> return NotThere
                        -- Not found
                        -- (If it is TRULY not found at all, we'll
                        -- error when we actually try to compile)


-- | The continuation to summarise a home module if we want to find the source file
-- for it and potentially compile it.
summariseModuleWithSource
          :: HomeUnit
          -> M.Map (UnitId, FilePath) ModSummary
          -- ^ Map of old summaries
          -> IsBootInterface    -- True <=> a {-# SOURCE #-} import
          -> Maybe (StringBuffer, UTCTime)
          -> HscEnv
          -> ModLocation
          -> Module
          -> IO SummariseResult
summariseModuleWithSource home_unit old_summary_map is_boot maybe_buf hsc_env location mod = do
        -- Adjust location to point to the hs-boot source file,
        -- hi file, object file, when is_boot says so
        let src_fn = expectJust (ml_hs_file location)

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
              Right ms -> FoundHome (ModuleNodeCompile ms)

  where
    dflags    = hsc_dflags hsc_env
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
  (pi_srcimps', pi_theimps', L pi_mod_name_loc pi_mod_name)
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
