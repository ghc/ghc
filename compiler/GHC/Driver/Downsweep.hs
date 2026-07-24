{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

-- | See Note [The ModuleGraph]
module GHC.Driver.Downsweep
  ( downsweep
  , downsweepThunk
  , downsweepInstalledModules
  , downsweepFromRootNodes
  , downsweepInteractiveImports
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
import GHC.Driver.DynFlags
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
import GHC.Runtime.Context

import Language.Haskell.Syntax.ImpExp

import GHC.Data.Graph.Directed
import GHC.Data.FastString
import GHC.Data.Maybe      ( expectJust )
import qualified GHC.Data.Maybe as M
import GHC.Data.OsPath     ( OsPath, unsafeEncodeUtf )
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
import GHC.Types.Basic


import GHC.Unit
import GHC.Unit.Env
import GHC.Unit.Finder
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Graph
import GHC.Unit.Module.Deps
import qualified GHC.Unit.Home.Graph as HUG
import GHC.Unit.Module.Stage

import Data.Either ( partitionEithers, lefts )
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
import Data.Bifunctor (first, bimap)
import System.Directory
import System.FilePath

import Control.Monad.Trans.Reader
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Class
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.IORef
import qualified Data.List.NonEmpty as NE

{-
Note [The ModuleGraph]
~~~~~~~~~~~~~~~~~~~~~~
The 'ModuleGraph' stores the relationship between all the modules, units, and
instantiations in the current session, allowing e.g. to answer questions about
the transitive closure of the imports.

* A /node/ of the `ModuleGraph`, of type `ModuleGraphNode`, corresponds
  1-1 with a home-package module of source code, N.hs or N.hs-boot.
  See the haddocks of `ModuleGraphNode`.

  The `ModuleNodeInfo` field of the `ModuleGraphNode` contains a `ModSummary`
  that in turn describes where the source file is (its `ModLocation`), when it
  was read, its contents etc. See Note [Module Types in the ModuleGraph].

  Each node has a distinct `NodeKey` (an instance of Ord); the function
        mkNodeKey :: ModuleGraphNode -> NodeKey
  get the `NodeKey` of a node

* An /edge/ of the `ModuleGraph` from N1 to N2 typically corresponds to a
  direct import of module N2 in module N1: one edge for each import.
  Imports of modules from non-home-packages are featured in the `ModuleGraph`
  as `UnitNode`s, or `InstantiationNodes` when backpack is involved.

  Each node contains a list of all its out-edges or, more precisely, of the
  `NodeKey`s of its direct dependencies.

Because a node in the `ModuleGraph` describes the precise dependencies of the module, each node has its
own `UnitId`.  Remember, a single module can be compiled against many different versions of a library; but
once we fix its dependencies we can compile it, and give it a `UnitId`.  See Note [About units] in GHC.Unit.

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

Note [Downsweep: building and maintaining the module graph]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The module graph can be built from scratch by starting from a set of /root nodes/
and exploring their dependencies. This is done by `GHC.Driver.Downsweep.downsweep`.

Another scenario is when we already /have/ a `ModuleGraph` and want to update
it (e.g. to reflect any file-system changes that have taken place since the
last invocation of `downsweep`) or augment it by exploring new roots (e.g. for
incrementally constructing a ModuleGraph using the GHC API; See #27054). So
`downsweep` takes a `Maybe ModuleGraph` as one of its arguments.

Downsweep iteratively *expands* each so-called 'DownsweepNode' into a list of
its dependencies, and recursively traverses all reachable nodes in a
depth-first order using 'dfsBuild'. A 'DownsweepNode' is *expanded* by 'dsNodeExpand':

  dsNodeExpand :: DownsweepNode -> DownsweepM (NodeRes (ModuleGraphNode, [DownsweepNode]))

Most notably:

  - 'DSMod' (Module-based) nodes can be expanded by preprocessing and
  parsing the module header, then listing the imports (direct and SOURCE imports)
  (see 'expandModuleSummary' and 'expandFixedModuleNode')

  - 'DSUnit' is expanded by finding the unit dependencies of that unit by id
  (see 'expandUnitNode').

Besides its dependencies, expanding a 'DownsweepNode' produces a
'ModuleGraphNode'. The final 'ModuleGraph' is constructed from the list of
'ModuleGraphNode's accumulated by expanding all reachable 'DownsweepNode's.

A 'ModuleGraphNode' is essentially the resolved version of 'DownsweepNode':
it records the payload (e.g. a Module) *and* its dependencies, unlike
'DownsweepNode' which has the just the payload that is used as a seed (and
potentially some context information, like the current home-unit)

TL;DR: We recursively traverse 'DownsweepNodes' to discover and build the 'ModuleGraph'.

See also Note [Downsweep Control Flow and Caching]
-}

-----------------------------------------------------------------------------
-- * Top-level entry to downsweep
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
-- Downsweeping can start from scratch or from a given module graph. In the
-- latter case, the given graph is fully included in the resulting graph, even
-- if parts of it are not reachable from any of the given roots. When an import
-- is processed, the source of the imported module is not consulted if this
-- module is already mentioned in the given graph. The sources of the root
-- modules are always consulted, though.
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
--
-- See also Note [The ModuleGraph]
downsweep :: HscEnv
          -> (GhcMessage -> AnyGhcDiagnostic)
          -> Maybe Messager
          -> [ModSummary]
          -- ^ Old summaries
          -> Maybe ModuleGraph
          -- ^ Optionally a module graph to extend
          -> [ModuleName]       -- Ignore dependencies on these; treat
                                -- them as if they were package modules
          -> Bool               -- True <=> allow multiple targets to have
                                --          the same module name; this is
                                --          very useful for ghc -M
          -> IO ([DriverMessages], ModuleGraph)
                -- The non-error elements of the returned list all have distinct
                -- (Modules, IsBoot) identifiers, unless the Bool is true in
                -- which case there can be repeats
downsweep hsc_env diag_wrapper msg old_summaries maybe_base_graph excl_mods allow_dup_roots = do
  n_jobs     <- mkWorkerLimit (hsc_dflags hsc_env)
  summ_cache <- newIORef (mkModSummaryCache (zip old_summaries (repeat SummOld)))
  imps_cache <- newIORef Map.empty
  (root_errs, root_summaries) <- rootSummariesParallel n_jobs hsc_env diag_wrapper msg
                                   (getRootSummary excl_mods summ_cache imps_cache)
  let closure_errs = checkHomeUnitsClosed unit_env
      unit_env = hsc_unit_env hsc_env

      all_errs = closure_errs ++ root_errs

  case all_errs of
    [] -> do
       (downsweep_errs, downsweep_nodes) <-
          downsweepFromRootNodes hsc_env summ_cache imps_cache maybe_base_graph
            excl_mods allow_dup_roots DownsweepUseCompile (map ModuleNodeCompile root_summaries) []

       let (other_errs, unit_nodes) = partitionEithers $
              HUG.unitEnv_foldWithKey (\nodes uid hue -> nodes ++ unitModuleNodes downsweep_nodes uid hue) []
                                      (hsc_HUG hsc_env)

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
    -- Dependencies arising on a unit (backpack and module linking deps)
    unitModuleNodes :: [ModuleGraphNode] -> UnitId -> HomeUnitEnv -> [Either (Messages DriverMessage) ModuleGraphNode]
    unitModuleNodes summaries uid hue =
      maybeToList (linkNodes summaries uid hue)

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

          do_linking =  main_sum || no_hs_main || ghcLink dflags == LinkDynLib || ghcLink dflags == LinkStaticLib || ghcLink dflags == LinkBytecodeLib

      in if | isExecutableLink (ghcLink dflags) && isJust ofile && not do_linking ->
                Just (Left $ singleMessage $ mkPlainErrorMsgEnvelope noSrcSpan (DriverRedirectedNoMain $ mainModuleNameIs dflags))
            -- This should be an error, not a warning (#10895).
            | ghcLink dflags /= NoLink, do_linking -> Just (Right (LinkNode unit_nodes uid))
            | otherwise  -> Nothing

-- | Calculate the module graph starting from a single ModSummary. The result is a
-- thunk, which when forced will perform the downsweep. This is useful in oneshot
-- mode where the module graph may never be needed.
-- If downsweep fails, then the resulting errors are just thrown.
downsweepThunk :: HscEnv -> ModSummary -> IO ModuleGraph
downsweepThunk hsc_env mod_summary = unsafeInterleaveIO $ do
  debugTraceMsg (hsc_logger hsc_env) 3 $ text "Computing Module Graph thunk..."
  summs <- newIORef (mkModSummaryCache [(mod_summary,SummOld)])
  imps  <- newIORef mempty
  ~(errs, mg) <- downsweepFromRootNodes hsc_env summs imps Nothing [] True DownsweepUseFixed [ModuleNodeCompile mod_summary] []
  let dflags = hsc_dflags hsc_env
  liftIO $ printOrThrowDiagnostics (hsc_logger hsc_env)
                                   (initPrintConfig dflags)
                                   (initDiagOpts dflags)
                                   (GhcDriverMessage <$> unionManyMessages errs)
  return (mkModuleGraph mg)

-- | Construct a module graph starting from the interactive context.
-- Produces, a thunk, which when forced will perform the downsweep.
-- This graph contains the current interactive module, and its dependencies.
--
--  Invariant: The hsc_mod_graph already contains the relevant home modules which
--  might be imported by the interactive imports.
--
-- This is a first approximation for this function. There probably should also
-- be edges linking the interactive modules together. (Ie Ghci7 importing Ghci6
-- and so on)
-- See Note [runTcInteractive module graph]
downsweepInteractiveImports :: HscEnv -> InteractiveContext -> IO ModuleGraph
downsweepInteractiveImports hsc_env ic = unsafeInterleaveIO $ do
  debugTraceMsg (hsc_logger hsc_env) 3 $ (text "Computing Interactive Module Graph thunk...")
  let imps = ic_imports (hsc_IC hsc_env)

      interactive_mn = icInteractiveModule ic
      key = dsNodeInfoKey (DSInteractive interactive_mn imps)

  -- The existing nodes in the module graph. This will be populated when GHCi runs
  -- :load. Any home package modules need to already be in here.
  let cached_nodes = Map.fromList [ (mkNodeKey n, NSuccess n) | n <- mg_mss (hsc_mod_graph hsc_env) ]

  summ_cache <- newIORef mempty
  imps_cache <- newIORef mempty
  let env = DownsweepEnv hsc_env DownsweepUseFixed{-or UseCompiled?-} summ_cache imps_cache []
  graph <- runDownsweepM env do
    loopFromInteractive cached_nodes interactive_mn imps
  let interactive_node  = case expectJust $ M.lookup key graph of
        NSuccess r -> r
        NSkip      -> pprPanic "downsweepInteractiveImports" (text "Skip")
      all_nodes  = [s | NSuccess s <- M.elems graph ]
  return $ mkModuleGraph (interactive_node : all_nodes)

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
    summs <- newIORef mempty
    imps  <- newIORef mempty
    (errs, mg) <- downsweepFromRootNodes hsc_env summs imps Nothing [] True DownsweepUseFixed nodes external_uids

    -- Similarly here, we should really not get any errors, but print them out if we do.
    let dflags = hsc_dflags hsc_env
    liftIO $ printOrThrowDiagnostics (hsc_logger hsc_env)
                                     (initPrintConfig dflags)
                                     (initDiagOpts dflags)
                                     (GhcDriverMessage <$> unionManyMessages errs)

    return (mkModuleGraph mg)

-----------------------------------------------------------------------------
-- * Orchestrator: downsweepFromRootNodes
-----------------------------------------------------------------------------

type ModSummaryCache = IORef ModSummaryCacheMap
type ImportsCache    = IORef ImportsCacheMap

-- | A cache from file paths to the already summarised modules. The same file
-- can be used in multiple units so the map is actually also keyed by which
-- unit the file was used in.
--
-- We want to reuse ModSummaries as far as possible because the most expensive
-- part of downsweep is reading and parsing the headers.
--
-- See Note [Downsweep Control Flow and Caching]
type ModSummaryCacheMap
      -- The cache can't be keyed by 'Module' because that isn't sufficient to
      -- distinguish .hs from .hs-boot files. Use path+unit instead.
      = ( M.Map (UnitId, OsPath) (Either DriverMessages (ModSummary, SummProvenance)) )

-- | A 'ModSummary's provenance during downsweep: an old previously constructed
-- ModSummary, that might be potentially outdated, or a freshly constructed one
-- during this downsweep which is certainly up to date?
data SummProvenance
  -- | Constructed during this downsweep: trivially up to date
  = SummFresh
  -- | Carried over from a previous run: may be stale, must be hash-checked
  -- (and considered by -fforce-recomp)
  | SummOld

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
                  -> ModSummaryCache
                  -> ImportsCache
                  -> Maybe ModuleGraph
                  -> [ModuleName]
                  -> Bool
                  -> DownsweepMode -- ^ Whether to create fixed or compile nodes for dependencies
                  -> [ModuleNodeInfo] -- ^ The starting ModuleNodeInfo
                  -> [UnitId] -- ^ The starting units
                  -> IO ([DriverMessages], [ModuleGraphNode])
downsweepFromRootNodes hsc_env summ_cache imps_cache maybe_base_graph excl_mods allow_dup_roots mode root_nodes root_uids = do
     when (not allow_dup_roots) $
       case root_duplicates of
         []           -> return ()
         (dup_root:_) -> multiRootsErr sec dup_root
     modifyImpsCache imps_cache (`M.union` mkRootMap root_nodes) -- add root nodes to imports cache
     let env = DownsweepEnv hsc_env mode summ_cache imps_cache excl_mods
     deps' <- runDownsweepM env  $ do
        let base_nodes = maybe M.empty moduleGraphNodeMap maybe_base_graph
        module_deps <- loopModuleNodeInfos base_nodes root_nodes
        all_deps    <- loopUnits module_deps (hscActiveUnitId hsc_env) root_uids
        deps'       <- loopInstantiations all_deps (getHomeUnitInstantiations hsc_env)
        return deps'
     f_cache <- readIORef summ_cache
     let downsweep_errs = lefts (M.elems f_cache)
         downsweep_nodes = [ s | NSuccess s <- M.elems deps' ]

     return (downsweep_errs, downsweep_nodes)
  where
    getHomeUnitInstantiations :: HscEnv -> [(UnitId, InstantiatedUnit)]
    getHomeUnitInstantiations hsc_env = HUG.unitEnv_foldWithKey
      (\nodes uid hue -> nodes ++  instantiationNodes uid (homeUnitEnv_units hue)) [] (hsc_HUG hsc_env)

    -- In a root module, the filename is allowed to diverge from the module
    -- name, so we have to check that there aren't multiple root files
    -- defining the same module (otherwise the duplicates will be silently
    -- ignored, leading to confusing behaviour).
    root_duplicates :: [NE.NonEmpty ModuleNodeInfo]
    root_duplicates = mapMaybe takes2 (M.elems root_map)
       where
         takes2 (a:as@(_:_)) = Just (a NE.:| as) -- Each at least of length 2
         takes2 _            = Nothing

         root_map = Map.fromListWith (flip (++))
           [ ((moduleNodeInfoUnitId s, moduleNodeInfoMnwib s), [s])
           | s <- root_nodes ]

    moduleGraphNodeMap :: ModuleGraph -> M.Map NodeKey (NodeRes ModuleGraphNode)
    moduleGraphNodeMap graph
        = M.fromList [(mkNodeKey node, NSuccess node) | node <- mgModSummaries' graph]

    sec = initSourceErrorContext (hsc_dflags hsc_env)

--------------------------------------------------------------------------------
-- ** 'DownsweepM'
--------------------------------------------------------------------------------

type DownsweepM a = ReaderT DownsweepEnv IO a
data DownsweepEnv = DownsweepEnv {
      downsweep_hsc_env :: HscEnv
    , _downsweep_mode :: DownsweepMode
    , _downsweep_summaries_cache :: ModSummaryCache
    , downsweep_imports_cache :: ImportsCache
    , _downsweep_excl_mods :: [ModuleName]
}

mkModSummaryCache :: [(ModSummary, SummProvenance)] -> ModSummaryCacheMap
mkModSummaryCache summs = foldl' (flip (uncurry addModSummaryCache)) M.empty summs

addModSummaryCache :: ModSummary -> SummProvenance -> ModSummaryCacheMap -> ModSummaryCacheMap
addModSummaryCache ms pr fe = upd_fe fe
  where
    upd_fe fe
      | Just src_fn_os <- ml_hs_file_ospath (ms_location ms)
      = M.insert (ms_unitid ms, src_fn_os) (Right (ms, pr)) fe
      | otherwise = fe

modifySummCache :: ModSummaryCache -> (ModSummaryCacheMap -> ModSummaryCacheMap) -> IO ()
modifyImpsCache :: ImportsCache    -> (ImportsCacheMap    -> ImportsCacheMap)    -> IO ()
modifySummCache r f = atomicModifyIORef' r (\c -> (f c, ()))
modifyImpsCache r f = atomicModifyIORef' r (\c -> (f c, ()))

-- | A cache from a module import (in given home unit context, with a package
-- qualifier, and the imported module name (with or without SOURCE)) to the
-- result of summarising that import (see 'summariseModuleDispatch').
--
-- See Note [Downsweep Control Flow and Caching]
type ImportsCacheMap
      = M.Map (UnitId, PkgQual, ModuleNameWithIsBoot) SummariseResult

-- | Populate the 'ImportsCacheMap' with the root modules.
mkRootMap :: [ModuleNodeInfo] -> ImportsCacheMap
mkRootMap summaries = Map.fromList
  [ ((moduleNodeInfoUnitId s, NoPkgQual, moduleNodeInfoMnwib s), FoundHome s) | s <- summaries ]

runDownsweepM :: DownsweepEnv -> DownsweepM a -> IO a
runDownsweepM env act = runReaderT act env

loopDownsweepNodes  :: M.Map NodeKey (NodeRes ModuleGraphNode) -> [DownsweepNode]               -> DownsweepM (M.Map NodeKey (NodeRes ModuleGraphNode))
loopModuleNodeInfos :: M.Map NodeKey (NodeRes ModuleGraphNode) -> [ModuleNodeInfo]              -> DownsweepM (M.Map NodeKey (NodeRes ModuleGraphNode))
loopUnits           :: M.Map NodeKey (NodeRes ModuleGraphNode) -> UnitId -> [UnitId]            -> DownsweepM (M.Map NodeKey (NodeRes ModuleGraphNode))
loopInstantiations  :: M.Map NodeKey (NodeRes ModuleGraphNode) -> [(UnitId, InstantiatedUnit)]  -> DownsweepM (M.Map NodeKey (NodeRes ModuleGraphNode))
loopFromInteractive :: M.Map NodeKey (NodeRes ModuleGraphNode) -> Module -> [InteractiveImport] -> DownsweepM (M.Map NodeKey (NodeRes ModuleGraphNode))
loopDownsweepNodes  base_map nodes = dfsBuild (Just base_map) nodes dsNodeInfoKey dsNodeExpand
loopModuleNodeInfos base_map       = loopDownsweepNodes base_map . map DSMod
loopUnits           base_map homud = loopDownsweepNodes base_map . map (DSUnit homud)
loopInstantiations  base_map       = loopDownsweepNodes base_map . map (uncurry DSInst)
loopFromInteractive base_map m     = loopDownsweepNodes base_map . (:[]) . DSInteractive m

--------------------------------------------------------------------------------
-- * Expanding 'DownsweepNode's into payload and node dependencies
--------------------------------------------------------------------------------

-- | A 'DownsweepNode' is the basic block of the downsweep algorithm which
-- encompasses the types of nodes we can iteratively expand to construct the
-- full module graph. See 'loopDownsweepNodes'.
--
-- See Note [Downsweep Control Flow and Caching]
data DownsweepNode
  -- | A module node to expand
  = DSMod ModuleNodeInfo
  -- | A unit node to expand
  | DSUnit
  { home_context_uid :: UnitId
  -- ^ The home unit which introduced the dependency on this 'node_uid'. This
  -- 'node_uid' can only be expanded in the context ('HscEnv') where
  -- 'home_context_uid' is the active home unit, to make sure the package flags
  -- are the ones attributed to the home package that introduced this node.
  , node_uid         :: UnitId
  -- ^ The unit node to expand
  }
  -- | FIXME: document the meaning of 'DSInst'
  | DSInst
  { home_context_uid :: UnitId
  , instantiated_ud  :: InstantiatedUnit
  }
  -- | A group of interactive imports from this interactive Module
  | DSInteractive Module [InteractiveImport]

instance Outputable DownsweepNode where
  ppr = \case
    DSMod (ModuleNodeCompile ms) -> text "DSModC" <+> ppr (ms_mod_name ms)
    DSMod (ModuleNodeFixed key _) -> text "DSModF" <+> ppr key
    DSUnit{node_uid} -> text "DSUnit" <+> ppr node_uid
    DSInst{instantiated_ud} -> text "DSInst" <+> ppr instantiated_ud
    DSInteractive mod ii    -> text "DSInteractive" <+> ppr mod <+> ppr ii

-- | They key by which to cache previously visited 'DownsweepNode's
dsNodeInfoKey :: DownsweepNode -> NodeKey
dsNodeInfoKey = \case
  DSMod (ModuleNodeCompile ms)  -> NodeKey_Module (msKey ms)
  DSMod (ModuleNodeFixed mod _) -> NodeKey_Module mod
  DSUnit{node_uid}              -> NodeKey_ExternalUnit node_uid
  DSInst{instantiated_ud}       -> NodeKey_Unit instantiated_ud
  DSInteractive mod _imps       -> NodeKey_Module $ moduleToMnk mod NotBoot

dsNodeExpand :: DownsweepNode -> DownsweepM (NodeRes (ModuleGraphNode, [DownsweepNode]))
dsNodeExpand = \case
  DSMod (ModuleNodeCompile ms)         -> expandModuleSummary ms
  DSMod (ModuleNodeFixed key loc)      -> expandFixedModuleNode key loc
  DSUnit{ node_uid, home_context_uid } -> expandUnitNode node_uid home_context_uid
  DSInst{ instantiated_ud
        , home_context_uid }           -> expandInstantiatedUnit instantiated_ud home_context_uid
  DSInteractive imod iis               -> expandInteractiveImports imod iis

expandModuleSummary :: ModSummary -> DownsweepM (NodeRes (ModuleGraphNode, [DownsweepNode]))
expandModuleSummary ms = do -- Didn't work out what the imports mean yet, now do that.
    hsc_env <- asks downsweep_hsc_env
    let home_uid  = ms_unitid ms
        home_unit = ue_unitHomeUnit home_uid (hsc_unit_env hsc_env)
    (final_deps, todo) <- unzip <$> mapM (expandModImport home_uid home_unit) (calcDeps ms)

    -- This has the effect of finding a .hs file if we are looking at the .hs-boot file.
    boot_todo <-
      if | HsBootFile <- ms_hsc_src ms
         -> do
            r <- downsweepSummarise home_unit NotBoot (noLoc $ ms_mod_name ms) NoPkgQual Nothing
            case r of
              FoundHome s -> pure [DSMod s]
              _           -> pure []
         | otherwise      -> pure []

    return $ NSuccess
      ( ModuleNode (catMaybes final_deps) (ModuleNodeCompile ms)
      , boot_todo ++ concat todo
      )
  where
    expandModImport home_uid home_unit (imp,mb_pkg,gwib) = do
      let GWIB { gwib_mod = L loc mod, gwib_isBoot = is_boot } = gwib
          wanted_mod = L loc mod
      mb_s <- downsweepSummarise home_unit is_boot wanted_mod mb_pkg Nothing
      case mb_s of
        NotThere -> return
          ( Nothing, [] )
        External uid -> return
          ( Just $ mkModuleEdge imp (NodeKey_ExternalUnit uid)
          -- Specify home unit, as each unit might have a different visible package database.
          , [DSUnit{node_uid = uid, home_context_uid = home_uid}] )
        FoundInstantiation iud -> return
          ( Just (mkModuleEdge imp (NodeKey_Unit iud)), [] )
        FoundHomeWithError (_uid, _e) -> return
          ( Nothing, [] )
          -- the error @e@ is already stored in the summarisation cache,
          -- (the IORef in DownsweepM) and will get reported at the end.
        FoundHome s -> return
          -- MP: This assumes that we can only instantiate non home units, which is probably fair enough for now.
          ( Just $ mkModuleEdge imp (NodeKey_Module (mnKey s))
          , [DSMod s] )

    calcDeps :: ModSummary -> [(ImportLevel, PkgQual, GenWithIsBoot (Located ModuleName))]
    calcDeps ms =
      -- Add a dependency on the HsBoot file if it exists
      -- This gets passed to the loopImports function which just ignores it if it
      -- can't be found.
      [(NormalLevel, NoPkgQual, GWIB (noLoc $ ms_mod_name ms) IsBoot) | NotBoot <- [isBootSummary ms] ] ++
      [(lvl, b, c) | (lvl, b, c) <- msDeps ms ]

-- | Expand a 'ModuleNodeFixed' node
-- NB: If you ever reach a Fixed node, everything under that also must be fixed.
expandFixedModuleNode :: ModNodeKeyWithUid -> ModLocation -> DownsweepM (NodeRes (ModuleGraphNode, [DownsweepNode]))
expandFixedModuleNode key loc = do
    hsc_env <- asks downsweep_hsc_env
    -- MP: TODO, we should just read the dependency info from the interface rather than either
    -- a. Loading the whole thing into the EPS (this might never nececssary and causes lots of things to be permanently loaded into memory)
    -- b. Loading the whole interface into a buffer before discarding it. (wasted allocation and deserialisation)
    read_result <- liftIO $
      -- 1. Check if the interface is already loaded into the EPS by some other
      -- part of the compiler.
      lookupIfaceByModuleHsc hsc_env (mnkToModule key) >>= \case
        Just iface -> return (M.Succeeded iface)
        Nothing -> readIface (hsc_hooks hsc_env) (hsc_logger hsc_env) (hsc_dflags hsc_env) (hsc_NC hsc_env) (mnkToModule key) (ml_hi_file loc)
    case read_result of
      M.Succeeded iface -> do
        -- Computer information about this node
        let node_deps = ifaceDeps (mi_deps iface)
            edges = map mkFixedEdge node_deps
            node = ModuleNode edges (ModuleNodeFixed key loc)
        deps' <- catMaybes <$> mapM (mk_dep hsc_env) (bimap snd snd <$> node_deps)
        pure $ NSuccess (node, deps')

      -- Skip any failure, we might try to read a .hi-boot file for
      -- example, even if there is not one.
      M.Failed {} ->
        pure NSkip
  where
    mk_dep hsc_env (Left key) = do
      -- Like expandImports, but we already know exactly which module we are looking for.
      read_result <- liftIO $ findExactModule hsc_env (mnkToInstalledModule key) (mnkIsBoot key)
      case read_result of
        InstalledFound loc -> do
          pure $ Just $ DSMod (ModuleNodeFixed key loc)
        _otherwise ->
          -- If the finder fails, just keep going, there will be another
          -- error later when we try to expand this dependency.
          pure Nothing
    mk_dep _ (Right uid_dep) = do
      -- Set active unit so that looking loopUnit finds the correct
      -- -package flags in the unit state.
      let home_uid = mnkUnitId key
      pure (Just DSUnit{node_uid=uid_dep, home_context_uid=home_uid})

    mkFixedEdge :: Either (ImportLevel, ModNodeKeyWithUid) (ImportLevel, UnitId) -> ModuleNodeEdge
    mkFixedEdge (Left (lvl, key))  = mkModuleEdge lvl (NodeKey_Module key)
    mkFixedEdge (Right (lvl, uid)) = mkModuleEdge lvl (NodeKey_ExternalUnit uid)

    ifaceDeps :: Dependencies -> [Either (ImportLevel, ModNodeKeyWithUid) (ImportLevel, UnitId)]
    ifaceDeps deps =
      [ Left (tcImportLevel lvl, ModNodeKeyWithUid dep uid)
      | (lvl, uid, dep) <- Set.toList (dep_direct_mods deps)
      ] ++
      [ Right (tcImportLevel lvl, uid)
      | (lvl, uid) <- Set.toList (dep_direct_pkgs deps)
      ]

-- | Expand a unit id under the context of a certain home unit
expandUnitNode :: UnitId {-^ @node_uid@ -} -> UnitId {-^ Home unit from where @node_uid@ was introduced -}
               -> DownsweepM (NodeRes (ModuleGraphNode, [DownsweepNode]))
expandUnitNode node_uid home_context_uid = do
    -- Set active unit so that looking loopUnit finds the correct
    -- -package flags in the unit state.
    hsc_env <- asks downsweep_hsc_env
    let lcl_hsc_env = hscSetActiveUnitId home_context_uid hsc_env
    case unitDepends <$> lookupUnitId (hsc_units lcl_hsc_env) node_uid of
      Just us -> pure $ NSuccess ((UnitNode us node_uid), map (\u -> DSUnit{node_uid=u, home_context_uid{-inherit-}}) us)
      Nothing -> pprPanic "loopUnit" (text "Malformed package database, missing " <+> ppr node_uid)

expandInstantiatedUnit :: InstantiatedUnit -> UnitId {-^ Home unit -} -> DownsweepM (NodeRes (ModuleGraphNode, [DownsweepNode]))
expandInstantiatedUnit iud home_uid = pure $ NSuccess
  ( InstantiationNode home_uid iud
  , [DSUnit{node_uid=instUnitInstanceOf iud, home_context_uid=home_uid}] )

expandInteractiveImports :: Module -> [InteractiveImport] -> DownsweepM (NodeRes (ModuleGraphNode, [DownsweepNode]))
expandInteractiveImports imod imps = do
  hsc_env    <- asks downsweep_hsc_env
  imps_cache <- asks downsweep_imports_cache

  let
    -- A simple edge to a module from the same home unit
    mkEdge (IIModule n) = return $
      let
        mod_node_key = ModNodeKeyWithUid
          { mnkModuleName = GWIB (moduleName n) NotBoot
          , mnkUnitId =
              -- 'toUnitId' is safe here, as we can't import modules that
              -- don't have a 'UnitId'.
              toUnitId (moduleUnit n)
          }
       in (Just $ ModuleNodeEdge NormalLevel (NodeKey_Module mod_node_key), [])

    -- A complete import statement
    mkEdge (IIDecl i) =
      let lvl = convImportLevel (ideclLevelSpec i)
          wanted_mod = unLoc (ideclName i)
          is_boot = ideclSource i
          mb_pkg = renameRawPkgQual (hsc_unit_env hsc_env) (unLoc $ ideclName i) (ideclPkgQual i)
          unitId = homeUnitId $ hsc_home_unit hsc_env
      in do
        let home_unit = ue_unitHomeUnit unitId (hsc_unit_env hsc_env)
        let k _ loc mod =
              let key = moduleToMnk mod is_boot
              in return $ FoundHome (ModuleNodeFixed key loc)

        found <- liftIO $ summariseModuleDispatch k hsc_env imps_cache
                            home_unit is_boot (noLoc wanted_mod) mb_pkg []
        case found of
          -- Case 1: Home modules have to already be in the cache.
          FoundHome (ModuleNodeFixed mod _) -> do
            let edge = ModuleNodeEdge lvl (NodeKey_Module mod)
            -- Note: Does not perform any further downsweep as the module must already be in the cache.
            return (Just edge, [])
          -- Case 2: External units may not be in the cache, if we haven't already initialised the
          -- module graph. We can construct the module graph for those here by calling loopUnit.
          External uid -> do
            let edge = ModuleNodeEdge lvl (NodeKey_ExternalUnit uid)
            return (Just edge, [DSUnit{node_uid=uid, home_context_uid=homeUnitId home_unit}])
          -- And if it's not found.. just carry on and hope.
          _ -> return (Nothing, [])

  (module_edges, todo) <- unzip <$> mapM mkEdge imps
  pure $ NSuccess
    ( ModuleNode (catMaybes module_edges) node_type, concat todo )
  where
    -- No sensible value for ModLocation.. if you hit this panic then you probably
    -- need to add proper support for modules without any source files to the driver.
    ml = pprPanic "modLocation" (ppr imod <+> ppr imps)
    key = moduleToMnk imod NotBoot
    node_type = ModuleNodeFixed key ml

--------------------------------------------------------------------------------
-- * Constructing Module Summaries
--------------------------------------------------------------------------------

downsweepSummarise :: HomeUnit
                   -> IsBootInterface
                   -> Located ModuleName
                   -> PkgQual
                   -> Maybe (StringBuffer, UTCTime)
                   -> DownsweepM SummariseResult
downsweepSummarise home_unit is_boot wanted_mod mb_pkg maybe_buf = do
  DownsweepEnv hsc_env mode summaries_cache_ref imports_cache_ref excl_mods <- ask
  liftIO $ case mode of
    DownsweepUseCompile ->
      summariseModule hsc_env home_unit summaries_cache_ref imports_cache_ref
                      is_boot wanted_mod mb_pkg maybe_buf excl_mods
    DownsweepUseFixed ->
      summariseModuleInterface hsc_env home_unit imports_cache_ref is_boot
                      wanted_mod mb_pkg excl_mods

multiRootsErr :: SourceErrorContext -> NE.NonEmpty ModuleNodeInfo -> IO ()
multiRootsErr sec (summ1 NE.:| summs)
  = throwOneError sec $ fmap GhcDriverMessage $
    mkPlainErrorMsgEnvelope noSrcSpan $ DriverDuplicatedModuleDeclaration mod files
  where
    mod = moduleNodeInfoModule summ1
    files = mapMaybe (ml_hs_file . moduleNodeInfoLocation) (summ1:summs)

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

getRootSummary ::
  [ModuleName] ->
  ModSummaryCache ->
  ImportsCache ->
  HscEnv ->
  Target ->
  IO (Either DriverMessages ModSummary)
getRootSummary excl_mods summ_cache imports_cache hsc_env target
  | TargetFile file mb_phase <- targetId
  = do
    let offset_file = augmentByWorkingDirectory dflags file
    exists <- liftIO $ doesFileExist offset_file
    if exists || isJust maybe_buf
    then summariseFile hsc_env home_unit summ_cache offset_file mb_phase
         maybe_buf
    else
      return $ Left $ singleMessage $
      mkPlainErrorMsgEnvelope noSrcSpan (DriverFileNotFound offset_file)
  | TargetModule modl <- targetId
  = do
    maybe_summary <- summariseModule hsc_env home_unit summ_cache imports_cache NotBoot
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

--------------------------------------------------------------------------------
-- * Check/validate properties and error out
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- * Enable Code Gen for Template Haskell
--------------------------------------------------------------------------------

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
      , Just enable_spec <- needs_codegen_map ms =
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
                                  EnableByteCode -> dflags { backend = bytecodeBackend }
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
    -- FIXME: Duplicated from makeDynFlagsConsistent
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

    (td_map, lookup_node) = mkStageDeps mod_graph

    queryReachable ns = isReachableMany td_map (mapMaybe lookup_node ns)

    -- NB: Do not inline these, it is very important to share them across all calls
    -- to needs_obj_set and needs_bc_set.
    !query_obj =
      let !deps = queryReachable need_obj_set
      in \k -> deps (expectJust $ lookup_node k)

    !query_bc  =
      let !deps = queryReachable need_bc_set
      in \k -> deps (expectJust $ lookup_node k)

    -- The direct dependencies of modules which require object code
    need_obj_set =

        -- Note we don't need object code for a module if it uses TemplateHaskell itself. Only
        -- it's dependencies.
        [ (mkNodeKey m, RunStage)
        | m@(ModuleNode _deps (ModuleNodeCompile ms)) <- mod_graph
        , isTemplateHaskellOrQQNonBoot ms
        , not (gopt Opt_UseBytecodeRatherThanObjects (ms_hspp_opts ms))
        ]

    -- The direct dependencies of modules which require byte code
    need_bc_set =
        [ (mkNodeKey m, RunStage)
        | m@(ModuleNode _deps (ModuleNodeCompile ms)) <- mod_graph
        , isTemplateHaskellOrQQNonBoot ms
        , gopt Opt_UseBytecodeRatherThanObjects (ms_hspp_opts ms)
        ]

    needs_obj_set, needs_bc_set :: ModNodeKeyWithUid -> Bool
    needs_obj_set k = query_obj (NodeKey_Module k, CompileStage)

    needs_bc_set k = query_bc  (NodeKey_Module k, CompileStage)

    -- A map which tells us how to enable code generation for a NodeKey
    needs_codegen_map :: ModSummary -> Maybe CodeGenEnable
    needs_codegen_map ms =
      let nk = msKey ms


      -- Another option here would be to just produce object code, rather than both object and
      -- byte code
      in case (needs_obj_set nk, needs_bc_set nk) of
        (True, True)   -> Just EnableByteCodeAndObject
        (True, False)  -> Just EnableObject
        (False, True)  -> Just EnableByteCode
        (False, False) -> Nothing

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

{- Note [-fno-code mode]
~~~~~~~~~~~~~~~~~~~~~~~~
GHC offers the flag -fno-code for the purpose of parsing and typechecking a
program without generating object files. This is intended to be used by tooling
and IDEs to provide quick feedback on any parser or type errors as cheaply as
possible.

When GHC is invoked with -fno-code, no object files or linked output will be
generated. As many errors and warnings as possible will be generated, as if
-fno-code had not been passed. The session DynFlags will have
backend == NoBackend.

-fwrite-interface
~~~~~~~~~~~~~~~~
Whether interface files are generated in -fno-code mode is controlled by the
-fwrite-interface flag. The -fwrite-interface flag is a no-op if -fno-code is
not also passed. Recompilation avoidance requires interface files, so passing
-fno-code without -fwrite-interface should be avoided. If -fno-code were
re-implemented today, there would be no need for -fwrite-interface as it
would considered always on; this behaviour is as it is for backwards compatibility.

================================================================
IN SUMMARY: ALWAYS PASS -fno-code AND -fwrite-interface TOGETHER
================================================================

Template Haskell
~~~~~~~~~~~~~~~~
A module using Template Haskell may invoke an imported function from inside a
splice. This will cause the type-checker to attempt to execute that code, which
would fail if no object files had been generated. See #8025. To rectify this,
during the downsweep we patch the DynFlags in the ModSummary of any home module
that is imported by a module that uses Template Haskell to generate object
code.

The flavour of the generated code depends on whether `-fprefer-byte-code` is enabled
or not in the module which needs the code generation. If the module requires byte-code then
dependencies will generate byte-code, otherwise they will generate object files.
In the case where some modules require byte-code and some object files, both are
generated by enabling `-fbyte-code-and-object-code`, the test "fat015" tests these
configurations.

The object files (and interface files if -fwrite-interface is disabled) produced
for Template Haskell are written to temporary files.

Note that since Template Haskell can run arbitrary IO actions, -fno-code mode
is no more secure than running without it.

Explicit Level Imports
~~~~~~~~~~~~~~~~~~~~~~
When `-XExplicitLevelImports` is enabled, code is only generated for modules
needed for the compile stage. The ReachabilityIndex created by `mkStageDeps` answers
the question, if I compile a module for a specific stage, then which modules at
other stages do I need. The roots of this query are the modules which use `TemplateHaskell`
at the runtime stage, and modules we need code generation for are those which
are needed at the compile time stage. All the logic about how ExplicitLevelImports
and TemplateHaskell affect the needed stages of a module is encoded in mkStageDeps.

Potential TODOS:
~~~~~
* Remove -fwrite-interface and have interface files always written in -fno-code
  mode
* Both .o and .dyn_o files are generated for template haskell, but we only need
  .dyn_o (for dynamically linked compilers) Fix it. (The needed way is 'hostFullWays')
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

-----------------------------------------------------------------------------
-- * Pre-processing and Summarising and modules
-----------------------------------------------------------------------------

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
        -> ModSummaryCache
        -> FilePath                     -- source file name
        -> Maybe Phase                  -- start phase
        -> Maybe (StringBuffer,UTCTime)
        -> IO (Either DriverMessages ModSummary)

summariseFile hsc_env' home_unit summ_cache_ref src_fn mb_phase maybe_buf
   = do file_summ_cache <- readIORef summ_cache_ref
        case M.lookup (homeUnitId home_unit, src_fn_os) file_summ_cache of
          Just (Right (chd_summary, SummFresh)) ->
            -- Fresh: use it straight away
            pure (Right chd_summary)
          Just (Right (old_summary, SummOld)) -> do
            -- we can use a cached summary if one is available and the
            -- source file hasn't changed,
            let location = ms_location $ old_summary

            src_hash <- get_src_hash
                    -- The file exists; we checked in getRootSummary above.
                    -- If it gets removed subsequently, then this
                    -- getFileHash may fail, but that's the right
                    -- behaviour.

                    -- return the cached summary if the source didn't change
            res <- checkSummaryHash
                hsc_env (new_summary src_fn)
                old_summary location src_hash
            case res of
              Right ms -> modifySummCache summ_cache_ref (addModSummaryCache ms SummFresh)
              Left _   -> pure ()
            return res
          _ -> do src_hash <- get_src_hash
                  new_summary src_fn src_hash
  where
    -- change the main active unit so all operations happen relative to the given unit
    hsc_env = hscSetActiveHomeUnit home_unit hsc_env'
    src_fn_os = unsafeEncodeUtf src_fn
    -- src_fn does not necessarily exist on the filesystem, so we need to
    -- check what kind of target we are dealing with
    get_src_hash = case maybe_buf of
                      Just (buf,_) -> return $ fingerprintStringBuffer buf
                      Nothing -> liftIO $ getFileHash src_fn

    new_summary src_fn src_hash = do
      res <- runExceptT $ do
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
      modifySummCache summ_cache_ref $ case res of
        Left e   -> M.insert (homeUnitId home_unit, src_fn_os) (Left e)
        Right ms -> addModSummaryCache ms SummFresh
      return res

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
           obj_timestamp <- modificationTimeIfExists (ml_obj_file_ospath location)

           -- We have to repopulate the Finder's cache for file targets
           -- because the file might not even be on the regular search path
           -- and it was likely flushed in depanal. This is not technically
           -- needed when we're called from sumariseModule but it shouldn't
           -- hurt.
           let fc      = hsc_FC hsc_env
               mod     = ms_mod old_summary
               hsc_src = ms_hsc_src old_summary
           addModuleToFinder fc mod location hsc_src

           hi_timestamp <- modificationTimeIfExists (ml_hi_file_ospath location)
           hie_timestamp <- modificationTimeIfExists (ml_hie_file_ospath location)

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
                -> ModSummaryCache
                -> ImportsCache
                -> IsBootInterface
                -> Located ModuleName
                -> PkgQual
                -> Maybe (StringBuffer, UTCTime)
                -> [ModuleName]
                -> IO SummariseResult
summariseModule hsc_env home_unit old_summaries imps_cache is_boot wanted_mod mb_pkg maybe_buf excl_mods =
  summariseModuleDispatch k hsc_env imps_cache home_unit is_boot wanted_mod mb_pkg excl_mods
  where
    k = summariseModuleWithSource home_unit old_summaries is_boot maybe_buf


-- | Like summariseModule but for interface files that we don't want to compile.
-- This version always returns a ModuleNodeFixed node.
summariseModuleInterface :: HscEnv
                        -> HomeUnit
                        -> ImportsCache
                        -> IsBootInterface
                        -> Located ModuleName
                        -> PkgQual
                        -> [ModuleName]
                        -> IO SummariseResult
summariseModuleInterface hsc_env home_unit imps_cache is_boot wanted_mod mb_pkg excl_mods =
  summariseModuleDispatch k hsc_env imps_cache home_unit is_boot wanted_mod mb_pkg excl_mods
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
          -> ImportsCache
          -> HomeUnit
          -> IsBootInterface    -- True <=> a {-# SOURCE #-} import
          -> Located ModuleName -- Imported module to be summarised
          -> PkgQual
          -> [ModuleName]               -- Modules to exclude
          -> IO SummariseResult


summariseModuleDispatch k hsc_env' imps_cache_ref home_unit is_boot (L _ wanted_mod) mb_pkg excl_mods
  | wanted_mod `elem` excl_mods
  = return NotThere
  | otherwise  = find_it
  where
    -- Temporarily change the currently active home unit so all operations
    -- happen relative to it
    hsc_env   = hscSetActiveHomeUnit home_unit hsc_env'

    find_it :: IO SummariseResult
    find_it = do
      imps_cache <- readIORef imps_cache_ref
      case M.lookup cache_key imps_cache of
        Just result -> return result
        Nothing -> do
          found <- findImportedModuleWithIsBoot hsc_env wanted_mod is_boot mb_pkg
          r <- case found of
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
          modifyImpsCache imps_cache_ref (M.insert cache_key r)
          return r

    cache_key = ( homeUnitId home_unit, mb_pkg
                , GWIB{ gwib_mod = wanted_mod, gwib_isBoot = is_boot })

-- | The continuation to summarise a home module if we want to find the source file
-- for it and potentially compile it.
summariseModuleWithSource
          :: HomeUnit
          -> ModSummaryCache
          -- ^ Cache of constructed summaries
          -> IsBootInterface    -- True <=> a {-# SOURCE #-} import
          -> Maybe (StringBuffer, UTCTime)
          -> HscEnv
          -> ModLocation
          -> Module
          -> IO SummariseResult
summariseModuleWithSource home_unit summ_cache_ref is_boot maybe_buf hsc_env location mod = do
    -- Adjust location to point to the hs-boot source file,
    -- hi file, object file, when is_boot says so
    let src_fn = expectJust (ml_hs_file location)
    summ_cache <- readIORef summ_cache_ref
    case ml_hs_file_ospath location >>= \p -> M.lookup (moduleUnitId mod, p) summ_cache of
      Just (Right (chd_summary, SummFresh)) ->
        -- Fresh! just return it
        pure $ FoundHome (ModuleNodeCompile chd_summary)

      Just (Left err) ->
        -- Failure, don't try to summarise it again
        pure $ FoundHomeWithError (moduleUnitId mod, err)

      mb_old -> do
        -- Either Nothing or a potentially old summary, must check.

        -- Check that it exists
        -- It might have been deleted since the Finder last found it
        maybe_h <- fileHashIfExists src_fn
        case maybe_h of
          -- This situation can also happen if we have found the .hs file but the
          -- .hs-boot file doesn't exist.
          Nothing -> return NotThere
          Just h  -> do
            fresult <- case mb_old of
              Just (Right (old_summary, SummOld)) ->
                -- check the hash on the source file, and return the cached
                -- summary if it hasn't changed. If the file has changed then
                -- need to resummarise.
                case maybe_buf of
                  Just (buf,_) ->
                      checkSummaryHash hsc_env (new_summary location mod src_fn) old_summary location (fingerprintStringBuffer buf)
                  Nothing    ->
                      checkSummaryHash hsc_env (new_summary location mod src_fn) old_summary location h
              Nothing ->
                new_summary location mod src_fn h
            return $ case fresult of
              Left err -> FoundHomeWithError (moduleUnitId mod, err)
              Right ms -> FoundHome (ModuleNodeCompile ms)
  where
    dflags    = hsc_dflags hsc_env
    new_summary :: ModLocation
                  -> Module
                  -> FilePath
                  -> Fingerprint
                  -> IO (Either DriverMessages ModSummary)
    new_summary location mod src_fn src_hash
      = do
        res <- runExceptT $ do
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
        modifySummCache summ_cache_ref $ case res of
          Left e -> case ml_hs_file_ospath location of
            Just p  -> M.insert (moduleUnitId mod, p) (Left e)
            Nothing -> id
          Right ms -> addModSummaryCache ms SummFresh
        return res

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
  obj_timestamp <- modificationTimeIfExists (ml_obj_file_ospath nms_location)
  dyn_obj_timestamp <- modificationTimeIfExists (ml_dyn_obj_file_ospath nms_location)
  hi_timestamp <- modificationTimeIfExists (ml_hi_file_ospath nms_location)
  hie_timestamp <- modificationTimeIfExists (ml_hie_file_ospath nms_location)
  bytecode_timestamp <- modificationTimeIfExists (ml_bytecode_file_ospath nms_location)
  extra_sig_imports <- findExtraSigImports hsc_env nms_hsc_src pi_mod_name

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
            ((,,) NormalLevel NoPkgQual . noLoc <$> extra_sig_imports) ++
            pi_theimps
        , ms_hs_hash = nms_src_hash
        , ms_iface_date = hi_timestamp
        , ms_hie_date = hie_timestamp
        , ms_obj_date = obj_timestamp
        , ms_dyn_obj_date = dyn_obj_timestamp
        , ms_bytecode_date = bytecode_timestamp
        }

data PreprocessedImports
  = PreprocessedImports
      { pi_local_dflags :: DynFlags
      , pi_srcimps  :: [Located ModuleName]
      , pi_theimps  :: [(ImportLevel, PkgQual, Located ModuleName)]
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
              sec = initSourceErrorContext pi_local_dflags
          mimps <- getImports popts sec imp_prelude pi_hspp_buf pi_hspp_fn src_fn
          return (first (mkMessages . fmap mkDriverPsHeaderMessage . getMessages) mimps)
  let rn_pkg_qual = renameRawPkgQual (hsc_unit_env hsc_env)
  let rn_imps = fmap (\(sp, pk, lmn@(L _ mn)) -> (sp, rn_pkg_qual mn pk, lmn))
  let pi_srcimps = pi_srcimps'
  let pi_theimps = rn_imps pi_theimps'
  return PreprocessedImports {..}

--------------------------------------------------------------------------------
-- * Generic traversal of iteratively-built graph: dfsBuild
--------------------------------------------------------------------------------

-- | The result of expanding a node in 'dfsBuild'.
data NodeRes v
  -- | Computed the node payload successfully
  = NSuccess v
  -- | Skip a node! This means this node doesn't produce a payload and we can
  -- just ignore it if we ever come across it.
  --
  -- In practice, this might happen because of an error or maybe from an
  -- attempt to expand e.g. an hs-boot node just to see if it sticks, but we
  -- don't distinguish these uses. Skip just means ignore this node and don't
  -- abort.
  | NSkip

-- | In a depth-first order, and starting from the given roots, traverse a
-- graph by iteratively expanding a node into a payload and a list of children
-- nodes to visit next.
--
-- A node is NEVER visited/expanded more than once, as long as the node key
-- @k@, computed from the node @n@, uniquely identifies that node.
--
-- The first argument @base_map@ is the starting set of already visited nodes
-- (these nodes won't be expanded again!).
--
-- The result is a mapping from the key of every node transitively reachable
-- from the root nodes (inclusively) to the payload returned by expanding that
-- node. The result includes the previously visited nodes given in @base_map@,
-- s.t. @dfsBuild base_map [] _ _ == base_map@.
--
-- The @expand@ function returns an 'NResult'. See the 'NResult' documentation
-- for more information about each result type.
--
-- Error handling and exiting early can be achieved by selecting a @Monad m@
-- accordingly, such as @Control.Monad.Except.Except@
--
-- Example usage: @n@ is instanced to @DownsweepNode@, @k@ is @NodeKey@, and @v@ is @ModuleNodeEdge@.
--
-- See also Note [Downsweep Control Flow and Caching]
dfsBuild :: (Ord k, Monad m)
         => Maybe (Map.Map k (NodeRes v))
         -- ^ Base map, existing results. We won't re-expand any of the nodes
         -- already present in this map.
         -> [n]
         -- ^ The root nodes from where to start traversal
         -> (n -> k)
         -- ^ Compute the key which uniquely identifies this node
         -> (n -> m (NodeRes (v,[n])))
         -- ^ Expand this node into its payload result and into the list of
         -- children nodes to visit next.
         -> m (Map.Map k (NodeRes v))
         -- ^ The result accumulates the payload of expanding the root nodes
         -- and all nodes transitively reachable from those roots.
dfsBuild base_map roots key expand = go roots (fromMaybe Map.empty base_map)
  where
    go []     visited = pure visited
    go (s:ss) visited
      | k `Map.member` visited
      = go ss visited
      | otherwise
      = do r <- expand s
           case r of
             NSkip ->
               go ss
                  (Map.insert k NSkip        visited) -- Skip!
             NSuccess (v,ns) ->
               go (ns ++ ss)
                  (Map.insert k (NSuccess v) visited)
      where
        k = key s

{-
Note [Downsweep Control Flow and Caching]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The control flow of downsweep is extracted into a single function `dfsBuild`,
which takes care of iteratively expanding and traversing all nodes of the
in-construction module graph necessary to build a full `ModuleGraph` at the
end.

There are three levels of caching going on, all of which are necessary to make
sure we don't do repeated work (notably, we NEVER summarise the same module
twice).

1. `dfsBuild` accumulates the final module graph and never revisits the
   same node of the module graph. Cache is keyed by the final
   `ModuleGraph`s `NodeKey`s.

    For example, suppose

       A imports B and C
       B imports D
       C imports D

    Then, starting from A we will expand A and push B and C to the worklist;
    then, going back to B, we expand B which pushes D to the worklist. After
    processing D, we go to C, which imports D, but we have already visited that
    module so we can just use the already-constructed `ModuleGraphNode` for D.

2. For Module A in home-unit u1, each import in the list of imports
   needs to be *found* (call to `findImportedModuleWithIsBoot`): at this
   point, we only have the `ModuleName` of the import, not the `Module`.
   This *finding* is somewhat expensive, so we cache it as well
   (`ImportsCache`). The cache key is the home-unit to which the module
   belongs~[1], the import package qualifier, and the ModuleName.

   Same example, suppose

      A imports B and C
      B imports D
      C imports D

   When expanding B, we will findImportedModule "import D".
   When expanding C, we would findImportedModule "import D", but we can just
   look it up in the cache

   [1] Different home-units will have different package flags, which means
   potentially different `Module` resolution for the same `ModuleName`.

3. The most expensive operation we want to avoid is summarising a
   `Module` into a `ModSummary`, which notably involves parsing the
   module header from scratch.
   The third cache, in essence, maps a `Module` to its `ModSummary`
   (named `ModSummaryCache`). This cache upholds the invariant: we NEVER
   summarise the same module twice. In practice, the cache key is the
   Module's UnitId and the Source path; the reason is we need to
   distinguish between `.hs` and `.hs-boot` files, as their summaries
   will differ.

   Note that this covers more than just (1), because we summarise all imports
   of a single module when expanding it (see 'expandModuleSummary'), before
   returning from the expansion function.

   Note that (2) can't guarantee this alone: Two ModuleName imports in
   separate units can (and likely do) map to the same `Module`.

See also Note [The ModuleGraph]
-}
