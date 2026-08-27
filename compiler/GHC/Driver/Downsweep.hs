{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

-- | See Note [The ModuleGraph]
module GHC.Driver.Downsweep
  ( downsweep
  , downsweepThunk
  , downsweepInstalledModules
  , downsweepInteractiveImports
  , DownsweepMode(..)
   -- * Reusing an earlier downsweep
  , CachedDownsweepResult(..)
  , emptyDownsweepCache
  , modSummaryReuseMap
   -- * The downsweep engine
  , DownsweepEnv(..)
  , DownsweepQuery
  , DownsweepAnswers
  , DownsweepRoots(..)
  , downsweepFromRootNodes
   -- * Summary functions
  , summariseSourceFile
  , SourceFileOptions(..)
  , defaultSourceFileOptions
  , SummProvenance(..)
  , PriorSummaries
  , SummariseResult(..)
  -- * Helper functions
  , instantiationNodes
  , checkHomeUnitsClosed
  -- * Enabling code generation for Template Haskell
  , enableCodeGenForTH
  ) where

import GHC.Prelude

import GHC.Platform.Ways

import GHC.Driver.Config.Finder (initFinderOpts)
import GHC.Driver.DynFlags
import GHC.Driver.Phases
import {-# SOURCE #-} GHC.Driver.Pipeline (preprocess)
import GHC.Driver.Session
import GHC.Driver.Backend
import GHC.Driver.Env
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Concurrency
import GHC.Driver.MakeAction
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Ppr

import GHC.Iface.Load

import GHC.Parser.Header
import GHC.Rename.Names
import GHC.Tc.Utils.Backpack
import GHC.Runtime.Context

import Language.Haskell.Syntax.ImpExp
import GHC.Types.UnresolvedImport

import GHC.Data.Dependent
import GHC.Data.Graph.Directed
import GHC.Data.FastString
import GHC.Data.Maybe      ( expectJust )
import qualified GHC.Data.Maybe as MaybeErr ( MaybeErr(..) )
import GHC.Data.OsPath     ( OsPath, unsafeDecodeUtf, unsafeEncodeUtf )
import GHC.Data.StringBuffer
import GHC.Data.Graph.Directed.Reachability

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
import GHC.Types.Unique.DSet ( uniqDSetToList )
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

import Control.Monad
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.Except ( ExceptT(..), runExceptT, throwE )
import Data.Either ( partitionEithers )
import Data.Foldable ( for_ )
import Data.Functor.Identity ( Identity(..) )
import Data.Maybe
import Data.List (partition, unfoldr)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time
import Data.Bifunctor (first)
import GHC.Generics ( Generic, Generically(..) )
import System.Directory ( doesFileExist )
import System.FilePath
import System.IO.Unsafe (unsafeInterleaveIO)

{- Note [The ModuleGraph]
~~~~~~~~~~~~~~~~~~~~~~~~~
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

See Note [Rules-based downsweep] for how downsweep computes the graph.
-}

-----------------------------------------------------------------------------
-- * Top-level entry to downsweep
-----------------------------------------------------------------------------

-- | Downsweep (dependency analysis) for @--make@ mode.
--
-- Chase downwards from the specified root set, returning summaries
-- for all home modules encountered.
--
-- The returned 'ModuleGraph' has one node for each home-package
-- module, plus one for any hs-boot files.  The imports of these nodes
-- are all there, including the imports of non-home-package modules.
--
-- This function is intended for use by @--make@ mode and will also insert
-- 'LinkNode's and 'InstantiationNode's for any home units.
--
-- See also Note [The ModuleGraph]
downsweep
  :: HscEnv
  -> CachedDownsweepResult
  -- ^ Re-usable information from a previous downsweep run
  -> [ModuleName]
  -- ^ Ignore dependencies on these; treat them as if they were package modules
  -> Bool
  -- ^ True <=> allow multiple targets to have the same module name
  -- (this is very useful for @ghc -M@); it is an error otherwise
  --
  -- NB: The returned module graph has exactly one node per key.
  -- When several root files are allowed to define the same module, only one
  -- of them is kept.
  -> IO ([DriverMessages], ModuleGraph)
downsweep hsc_env cached_res excl_mods allow_dup_roots =
  do
    let prior_summaries = Map.map (\ ms -> (ms, SummOld)) (cdr_summaries cached_res)

        mk_env known prior = DownsweepEnv
          { ds_hsc_env   = hsc_env
          , ds_mode      = DownsweepUseCompile known
          , ds_prior     = prior
          , ds_excl_mods = excl_mods
          }

        (file_targets, module_targets) =
          partition is_file_target (hsc_targets hsc_env)

    -- First summarise the file targets: they may live off the search path and
    -- declare a module name unrelated to their file name, so their modules
    -- can only be known from their summaries.
    (file_errs, file_root_summaries) <-
      rootSummariesParallel (mk_env emptyKnownHomeModules prior_summaries)
        file_targets

    let known = knownHomeModulesOfSummaries (map rs_summary file_root_summaries)
        -- The root summaries have just been computed, so they are used as they
        -- are; summaries carried over from a previous downsweep are only
        -- candidates, checked against their source hash before use.
        fresh root_summaries =
          Map.union
            ( Map.fromList [ (rs_file rs, (rs_summary rs, SummFresh)) | rs <- root_summaries ] )
            prior_summaries

    -- Only now start to resolve module targets, which may name modules
    -- that the file targets provide.
    (module_errs, module_root_summaries) <-
      rootSummariesParallel (mk_env known (fresh file_root_summaries))
        module_targets

    let root_errs      = file_errs ++ module_errs
        root_summaries = file_root_summaries ++ module_root_summaries

    let unit_env     = hsc_unit_env hsc_env
        closure_errs = checkHomeUnitsClosed unit_env
        all_errs     = closure_errs ++ root_errs

    case all_errs of
      _ : _ -> return (all_errs, emptyMG)
      [] -> do
         when (not allow_dup_roots) $
           case root_duplicates (map rs_summary root_summaries) of
             []           -> return ()
             (dup_root:_) -> multiRootsErr (sec hsc_env) dup_root

         let env = mk_env known (fresh root_summaries)
             roots = foldMap rootSummaryRoots root_summaries

         (downsweep_errs, downsweep_nodes, _answers) <-
           downsweepFromRootNodes env cached_res roots

         let (other_errs, unit_nodes) = partitionEithers $
                HUG.unitEnv_foldWithKey (\nodes uid hue -> nodes ++ unitModuleNodes downsweep_nodes uid hue) []
                                        (hsc_HUG hsc_env)

         let all_nodes = downsweep_nodes ++ unit_nodes
         let final_errs = downsweep_errs ++ other_errs

         return (final_errs, mkModuleGraph all_nodes)
  where
    is_file_target :: Target -> Bool
    is_file_target (Target { targetId }) =
      case targetId of
        TargetFile {}   -> True
        TargetModule {} -> False

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

    -- In a root module, the filename is allowed to diverge from the module
    -- name, so we have to check that there aren't multiple root files
    -- defining the same module.
    root_duplicates :: [ModSummary] -> [NE.NonEmpty ModSummary]
    root_duplicates summaries = mapMaybe takes2 (Map.elems root_map)
       where
         takes2 (a:as@(_:_)) = Just (a NE.:| as) -- Each at least of length 2
         takes2 _            = Nothing

         root_map = Map.fromListWith (flip (++)) [ (msKey s, [s]) | s <- summaries ]

    sec hsc_env = initSourceErrorContext (hsc_dflags hsc_env)

-- | Calculate the module graph starting from a single ModSummary. The result is a
-- thunk, which when forced will perform the downsweep. This is useful in oneshot
-- mode where the module graph may never be needed.
--
-- If downsweep fails, then the resulting errors are just thrown.
downsweepThunk :: HscEnv -> ModSummary -> IO ModuleGraph
downsweepThunk hsc_env mod_summary = unsafeInterleaveIO $ do
  debugTraceMsg (hsc_logger hsc_env) 3 $ text "Computing Module Graph thunk..."
  let root_path = expectJust $ ml_hs_file_ospath (ms_location mod_summary)
      root_key = (ms_unitid mod_summary, root_path)
      env = DownsweepEnv
        { ds_hsc_env   = hsc_env
        , ds_mode      = DownsweepUseFixed
        , ds_prior     = Map.singleton root_key (mod_summary, SummFresh)
        , ds_excl_mods = []
        }
  (errs, nodes, _) <-
    downsweepFromRootNodes env emptyDownsweepCache
      (mempty { dr_summarise = [root_key] })
  let dflags = hsc_dflags hsc_env
  printOrThrowDiagnostics (hsc_logger hsc_env)
                          (initPrintConfig dflags)
                          (initDiagOpts dflags)
                          (GhcDriverMessage <$> unionManyMessages errs)
  return (mkModuleGraph nodes)

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
  debugTraceMsg (hsc_logger hsc_env) 3 $ text "Computing Interactive Module Graph thunk..."

  -- The existing nodes in the module graph. This will be populated when GHCi runs
  -- :load. Any home package modules need to already be in here.
  let cached_nodes = Map.fromList [ (mkNodeKey n, n) | n <- mg_mss (hsc_mod_graph hsc_env) ]
      home_uid = homeUnitId (hsc_home_unit hsc_env)
      env = DownsweepEnv
        { ds_hsc_env   = hsc_env
        , ds_mode      = DownsweepUseFixed
        , ds_prior     = Map.empty
        , ds_excl_mods = []
        }
      (direct_edges, lookups) = partitionEithers $ map interactiveImport (ic_imports ic)

  (_errs, nodes, answers) <-
    downsweepFromRootNodes env
      ( emptyDownsweepCache { cdr_nodes = cached_nodes } )
      ( mempty { dr_resolve = [ (home_uid, lkp) | (_, lkp) <- lookups ] } )

  let interactive_edges =
        direct_edges ++
        [ mkModuleEdge lvl target
        | (lvl, lkp) <- lookups
        , Just target <- [ downsweepEdgeTarget answers home_uid lkp ] ]
      interactive_key = moduleToMnk (icInteractiveModule ic) NotBoot
      -- No sensible value for ModLocation.. if you hit this panic then you
      -- probably need to add proper support for modules without any source
      -- files to the driver.
      interactive_loc = pprPanic "modLocation" (ppr interactive_key)
      interactive_node =
        ModuleNode interactive_edges (ModuleNodeFixed interactive_key interactive_loc)
  return $ mkModuleGraph (interactive_node : nodes)
  where
    -- A module named with @:module@ is an edge to a module whose identity we
    -- already know; a complete import declaration has to be resolved first.
    interactiveImport :: InteractiveImport -> Either ModuleNodeEdge (ImportLevel, UnresolvedImport PkgQual)
    interactiveImport (IIModule n) =
      Left $ mkNormalEdge $ NodeKey_Module $
        ModNodeKeyWithUid
          { mnkModuleName = GWIB (moduleName n) NotBoot
            -- 'toUnitId' is safe here, as we can't import modules that
            -- don't have a 'UnitId'.
          , mnkUnitId     = toUnitId (moduleUnit n)
          }
    interactiveImport (IIDecl i) =
      let imp = rnUnresolvedImportPkgQual (renameRawPkgQual (hsc_unit_env hsc_env))
                                          (unLoc (mkUnresolvedImport i))
      in Right (ui_level imp, imp)

-- | Create a module graph from a list of installed modules.
-- This is used by the loader when we need to load modules but there
-- isn't already an existing module graph. For example, when loading plugins
-- during initialisation.
--
-- If you call this function, then if the `Module` you request to downsweep can't
-- be found then this function will throw errors.
downsweepInstalledModules :: HscEnv -> [Module] -> IO ModuleGraph
downsweepInstalledModules hsc_env mods = do
    let
        (home_mods, external_mods) = partition (\u -> moduleUnitId u `elem` hsc_all_home_unit_ids hsc_env) mods
        installed_mods = map (fst . getModuleInstantiation) home_mods
        external_uids = map moduleUnitId external_mods

    -- It is an internal-ish error if a module cannot be found, since any call
    -- to this function should already know that we can find the modules we need
    -- to load.
    for_ installed_mods $ \ i ->
      runFinderM (findExactModule hsc_env i NotBoot) >>= \case
        InstalledFound {} -> return ()
        _ -> throwGhcException $ ProgramError $ showSDoc (hsc_dflags hsc_env) $
               text "downsweepInstalledModules: Could not find installed module" <+> ppr i

    let env = DownsweepEnv
          { ds_hsc_env   = hsc_env
          , ds_mode      = DownsweepUseFixed
          , ds_prior     = Map.empty
          , ds_excl_mods = []
          }
        roots = mempty
          { dr_fixed = map installedModuleToMnk installed_mods
          , dr_units = [ (hscActiveUnitId hsc_env, uid) | uid <- external_uids ]
          }
    (errs, nodes, _) <- downsweepFromRootNodes env emptyDownsweepCache roots

    -- Similarly here, we should really not get any errors, but print them out if we do.
    let dflags = hsc_dflags hsc_env
    printOrThrowDiagnostics (hsc_logger hsc_env)
                            (initPrintConfig dflags)
                            (initDiagOpts dflags)
                            (GhcDriverMessage <$> unionManyMessages errs)

    return (mkModuleGraph nodes)

--------------------------------------------------------------------------------
-- * Downsweep queries
--------------------------------------------------------------------------------

-- | One unit of work for downsweep: the inputs and outputs of a query.
--
-- See Note [Rules-based downsweep].
data DownsweepQuery answer where
  -- | Resolve an import.
  Resolve :: !UnitId -> !(UnresolvedImport PkgQual) -> DownsweepQuery ImportResolution
  -- | Preprocess a home unit source file and parse its header.
  Summarise :: !UnitId -> !OsPath -> DownsweepQuery SummariseResult
  -- | Read the interface of a home module that is taken as given.
  Fixed :: !ModNodeKeyWithUid -> DownsweepQuery (Maybe FixedModule)
  -- | Compute the direct dependencies of an external unit, as seen from a home unit.
  UnitDeps
    :: !UnitId -- ^ home unit
    -> !UnitId -- ^ query unit
    -> DownsweepQuery [UnitId]
  -- | Perform Backpack instantiation.
  Instantiate
    :: !UnitId           -- ^ home unit
    -> !InstantiatedUnit -- ^ query unit
    -> DownsweepQuery ()

-- | The collection of all answers computed by downsweep.
type DownsweepAnswers = DMap DownsweepQuery Identity

-- | What an import statement was resolved to.
data ImportResolution
  -- | A home module, to be summarised from the given source file.
  --
  -- The module is the one the finder resolved to, which need not be the one the
  -- import named: a unit can re-export a module under another name.
  = ResolvedHome !ModNodeKeyWithUid !OsPath
  -- | A home module whose interface is taken as given.
  | ResolvedFixed !ModNodeKeyWithUid
  -- | A module of an external unit.
  | ResolvedExternal !UnitId
  -- | A module of a Backpack instantiation.
  | ResolvedInstantiation !InstantiatedUnit
  -- | The import leads nowhere, and so contributes no edge.
  --
  -- If it is truly not found at all, we will error when we actually try to
  -- compile the importing module.
  | ResolvedNotFound

-- | The result of summarising a home unit source file.
data SummariseResult
  -- | There is no such file.
  --
  -- Not necessarily an error: for example, we speculatively look up the @hs-boot@
  -- file of every non-boot module, and this is the result we get when the module
  -- does not have an @hs-boot@ file.
  = SummariseNotThere
  -- | The file could not be summarised.
  | SummariseFailed !DriverMessages
  | SummariseFound !ModSummary

-- | A home module taken as given rather than compiled: the interface file that
-- stands for it, and the edges recorded in it.
data FixedModule =
  FixedModule
    { fixed_location :: !ModLocation
    , fixed_edges    :: ![ModuleNodeEdge]
    }

instance GEq DownsweepQuery where
  geq a b = case gcompare a b of
    GEQ -> Just Refl
    GLT -> Nothing
    GGT -> Nothing

instance GCompare DownsweepQuery where
  gcompare (Resolve u1 l1) (Resolve u2 l2) = gcompareOrd (u1, l1) (u2, l2)
  gcompare Resolve {} _ = GLT
  gcompare _ Resolve {} = GGT
  gcompare (Summarise u1 p1) (Summarise u2 p2) = gcompareOrd (u1, p1) (u2, p2)
  gcompare Summarise {} _ = GLT
  gcompare _ Summarise {} = GGT
  gcompare (Fixed k1) (Fixed k2) = gcompareOrd k1 k2
  gcompare Fixed {} _ = GLT
  gcompare _ Fixed {} = GGT
  gcompare (UnitDeps h1 u1) (UnitDeps h2 u2) = gcompareOrd (h1, u1) (h2, u2)
  gcompare UnitDeps {} _ = GLT
  gcompare _ UnitDeps {} = GGT
  gcompare (Instantiate h1 i1) (Instantiate h2 i2) = gcompareOrd (h1, i1) (h2, i2)

instance Outputable (DownsweepQuery answer) where
  ppr = \case
    Resolve uid lkp    -> text "resolve" <+> ppr lkp <+> text "from" <+> ppr uid
    Summarise uid p    -> text "summarise" <+> ppr uid <> colon <> text (unsafeDecodeUtf p)
    Fixed key          -> text "fixed" <+> ppr key
    UnitDeps home uid  -> text "unit deps" <+> ppr uid <+> text "from" <+> ppr home
    Instantiate home i -> text "instantiate" <+> ppr i <+> text "from" <+> ppr home

--------------------------------------------------------------------------------
-- * The downsweep environment
--------------------------------------------------------------------------------

-- | Whether downsweep should use compile or fixed nodes for the home modules it
-- reaches.
--
-- See Note [Module Types in the ModuleGraph] for the difference between the two.
data DownsweepMode
  -- | @--make@: home modules are summarised from source and compiled.
  = DownsweepUseCompile
      !KnownHomeModules
        -- ^ Known home modules computed from the file-target roots of
        -- downsweep.
  -- | Home modules are taken from their interface files (one-shot mode), or
  -- from the module graph when already present there (GHCi's interactive
  -- imports, see Note [runTcInteractive module graph]). Resolution sees the
  -- session's current known home modules, which remain authoritative.
  --
  -- Whether a module actually exists is decided by its 'Fixed' query: a
  -- module whose interface cannot be found or read produces no graph node,
  -- and imports resolving to it produce no edge.
  | DownsweepUseFixed

-- | A 'ModSummary's provenance during downsweep: an old previously constructed
-- ModSummary, that might be potentially outdated, or a freshly constructed one
-- during this downsweep which is certainly up to date?
data SummProvenance
  -- | Constructed during this downsweep: trivially up to date
  = SummFresh
  -- | Carried over from a previous run: may be stale, must be hash-checked
  -- (and considered by -fforce-recomp)
  | SummOld

-- | Summaries of source files that downsweep does not have to make itself.
--
-- The same file can be used in several units, so this is keyed by unit as well
-- as by path. Reading and parsing module headers is the most expensive part of
-- downsweep, which is what makes carrying these around worthwhile.
type PriorSummaries = Map.Map (UnitId, OsPath) (ModSummary, SummProvenance)

-- | Everything the downsweep rules read. Immutable: an answer is a function of
-- its query and of this.
data DownsweepEnv =
  DownsweepEnv
    { ds_hsc_env   :: !HscEnv
    , ds_mode      :: !DownsweepMode
      -- ^ Whether to create fixed or compile nodes for the modules we reach
    , ds_prior     :: !PriorSummaries
    , ds_excl_mods :: ![ModuleName]
      -- ^ Ignore imports of these; treat them as if they were package modules
    }

-- | The environment that module resolution runs in: the known home modules of
-- a compile-mode downsweep override the session's (see 'DownsweepUseCompile').
resolveImportHscEnv :: DownsweepEnv -> HscEnv
resolveImportHscEnv (DownsweepEnv { ds_hsc_env, ds_mode }) = case ds_mode of
  DownsweepUseCompile known -> setKnownHomeModules known ds_hsc_env
  DownsweepUseFixed         -> ds_hsc_env

-- | The queries a downsweep starts from.
--
-- Combine roots with the pointwise 'Monoid', so that each root can be
-- described on its own and the contributions folded together
-- (e.g. 'rootSummaryRoots').
data DownsweepRoots =
  DownsweepRoots
    { dr_summarise   :: ![(UnitId, OsPath)]
      -- ^ Home unit source files to summarise
    , dr_fixed       :: ![ModNodeKeyWithUid]
      -- ^ Home modules to take as given
    , dr_units       :: ![(UnitId, UnitId)]
      -- ^ External units to expand, in the given home unit context
    , dr_resolve     :: ![(UnitId, UnresolvedImport PkgQual)]
      -- ^ Module lookups to resolve, in the given home unit context
    , dr_resolutions :: ![(UnitId, UnresolvedImport PkgQual, ImportResolution)]
      -- ^ Module lookups already resolved (e.g. module targets).
      --
      -- NB: unlike the 'cdr_nodes' cache, these seeded resolutions __do__
      -- cause their dependencies to be visited.
    }
  deriving stock Generic
  deriving ( Semigroup, Monoid ) via Generically DownsweepRoots

-- | The parts of an earlier downsweep that a new downsweep can reuse.
data CachedDownsweepResult =
  CachedDownsweepResult
    { cdr_summaries :: !(Map.Map (UnitId, OsPath) ModSummary)
      -- ^ Candidate cached module summaries.
      --
      -- Only candidates: each module summmary is checked against its source
      -- hash and resummarised if stale.
    , cdr_nodes     :: !(Map.Map NodeKey ModuleGraphNode)
      -- ^ Cached module graph nodes.
      --
      -- These cached module graph nodes shortcut: no nodes beneath them are
      -- discovered.
    }

emptyDownsweepCache :: CachedDownsweepResult
emptyDownsweepCache = CachedDownsweepResult Map.empty Map.empty

-- | Use the given module summaries as a cache.
modSummaryReuseMap :: [ModSummary] -> Map.Map (UnitId, OsPath) ModSummary
modSummaryReuseMap summaries =
  Map.fromList
    [ ((ms_unitid ms, path), ms)
    | ms <- summaries
    , Just path <- [ml_hs_file_ospath (ms_location ms)] ]

--------------------------------------------------------------------------------
-- * Running the downsweep
--------------------------------------------------------------------------------

-- | Perform downsweep, starting from the given roots.
--
-- This function will start at the given roots and traverse downwards to find
-- all the dependencies, all the way to the leaf units.
--
-- See Note [Rules-based downsweep].
downsweepFromRootNodes
  :: DownsweepEnv
  -> CachedDownsweepResult
  -> DownsweepRoots
  -> IO ([DriverMessages], [ModuleGraphNode], DownsweepAnswers)
downsweepFromRootNodes env cached_res roots = do
  pool_config <- hscWorkerPool "downsweep_worker" (ds_hsc_env env)
  answers <- runRules pool_config known rule_set
  let (errs, nodes) = moduleGraphFromDownsweepAnswers (cdr_nodes cached_res) answers
  return (errs, Map.elems nodes, answers)
  where
    rule_set =
      RuleSet
        { runRule = downsweepRule env
        , initialDemands = \ demand -> do
            for_ (dr_summarise roots) $ \ (uid, path) -> demand (Summarise uid path)
            for_ (dr_fixed roots)     $ \ key         -> demand (Fixed key)
            for_ (dr_units roots)     $ \ (home, uid) -> demand (UnitDeps home uid)
            for_ (dr_resolve roots)   $ \ (home, lkp) -> demand (Resolve home lkp)
            -- A seeded resolution's rule never runs, so push the demands it
            -- would have pushed.
            for_ (dr_resolutions roots) $ \ (home, _, res) ->
              demandResolution demand home res
            for_ (hugInstantiations (ds_hsc_env env)) $ \ (uid, iud) ->
              demand (Instantiate uid iud)
        }

    known = fromListDMap $
      [ Resolve home lkp :=> Identity res | (home, lkp, res) <- dr_resolutions roots ]
      ++ concatMap seeded (Map.elems (cdr_nodes cached_res))

    seeded :: ModuleGraphNode -> [DSum DownsweepQuery Identity]
    seeded node = case node of
      ModuleNode edges (ModuleNodeCompile ms) ->
        [ Summarise (ms_unitid ms) path :=> Identity (SummariseFound ms)
        | Just path <- [ml_hs_file_ospath (ms_location ms)] ]
        ++
        [ Fixed (msKey ms) :=> Identity (Just (FixedModule (ms_location ms) edges)) ]
      ModuleNode edges (ModuleNodeFixed key loc) ->
        [ Fixed key :=> Identity (Just (FixedModule loc edges)) ]
      InstantiationNode home iud ->
        [ Instantiate home iud :=> Identity () ]
      UnitNode { un_home_uid, un_deps, un_uid } ->
        [ UnitDeps un_home_uid un_uid :=> Identity un_deps ]
      LinkNode {} -> []

-- | The instantiations wanted by every home unit of the session.
hugInstantiations :: HscEnv -> [(UnitId, InstantiatedUnit)]
hugInstantiations hsc_env = HUG.unitEnv_foldWithKey
  (\nodes uid hue -> nodes ++ instantiationNodes uid (homeUnitEnv_units hue)) [] (hsc_HUG hsc_env)

{- Note [Rules-based downsweep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Downsweep computes the module graph by answering queries. A query is specified
by the 'DownsweepQuery' type. Example queries:

  - Resolve: resolve an import declaration to a specific module.
  - Summarise: preprocess a source file and parse its module header.

'downsweepRule' specifies how to answer each query, by using a map keyed by
query to store all of the already computed query answers
(see 'GHC.Driver.Concurrency.runRules').

To achieve maximum concurrency, a rule that discovers more work (e.g. the imports
of a module whose header we just parsed) will demand additional queries to be
performed without waiting on them to complete.

Downsweep proceeds by starting from a set of roots ('DownsweepRoots'), and
iteratively answering queries that might each trigger additional demands. Once
all demands have been answered, we turn the final set of answers into the a
'ModuleGraph': see 'moduleGraphFromDownsweepAnswers'.

'CachedDownsweepResult' allows the result of a previous downsweep pass to be
re-used.
-}

--------------------------------------------------------------------------------
-- * The rules
--------------------------------------------------------------------------------

-- | How to answer each downsweep query.
downsweepRule :: DownsweepEnv -> Demand DownsweepQuery -> Rule DownsweepQuery
downsweepRule env demand query =
  case query of

    Resolve home_uid lkp -> do
      mbCached <- runFinderCacheM $ resolveDownsweepImport env home_uid lkp
      case mbCached of
        InCache resolution -> do
          demandResolution demand home_uid resolution
          return $ AnswerInline resolution
        NotInCache finder_action -> return $ AnswerDefer \ _worker_env -> do
          -- The finder search action never interacts with the LogQueue nor the
          -- TmpFs, so it's OK to ignore the 'ConcurrentWorkerEnv' here (which
          -- otherwise overrides them with local values for the worker).
          resolution <- finder_action
          demandResolution demand home_uid resolution
          return resolution

    Summarise uid path -> pure $ AnswerDefer \ worker_env -> do
      result <- summariseHomeSourceFile (worker_local_env worker_env) uid path
      case result of
        SummariseFound ms ->
          for_ (moduleDiscoveries ms) $ \ imp ->
            demand (Resolve uid imp)
        SummariseNotThere  -> return ()
        SummariseFailed {} -> return ()
      return result

    Fixed key -> pure $ AnswerDefer \ worker_env -> do
      result <- readFixedModule (worker_local_env worker_env) key
      for_ result $ \ (FixedModule { fixed_edges }) ->
        for_ fixed_edges $ \ edge -> case edgeTargetKey edge of
          NodeKey_Module dep_key       -> demand (Fixed dep_key)
          -- The home unit context decides which -package flags the unit is
          -- expanded with.
          NodeKey_ExternalUnit dep_uid -> demand (UnitDeps (mnkUnitId key) dep_uid)
          NodeKey_Unit {}              -> return ()
          NodeKey_Link {}              -> return ()
      return result

    -- Pure lookup in the package database: perform the computation inline.
    UnitDeps home_uid uid -> do
      let deps = externalUnitDepends env home_uid uid
      for_ deps $ \ dep -> demand (UnitDeps home_uid dep)
      return $ AnswerInline deps

    Instantiate home_uid iud -> do
      demand (UnitDeps home_uid (instUnitInstanceOf iud))
      -- An instantiation node depends on the home unit's implementation of each
      -- hole it fills, so those modules have to be visited too.
      for_ (uniqDSetToList (instUnitHoles iud)) $ \ hole ->
        demand (Resolve home_uid (generatedImport LookupUser hole))
      return $ AnswerInline ()
  where
    worker_local_env :: ConcurrentWorkerEnv -> DownsweepEnv
    worker_local_env worker_env =
      env { ds_hsc_env = setHscWorkerEnv worker_env (ds_hsc_env env) }

-- | Demand the query a resolution leads to.
demandResolution :: Demand DownsweepQuery -> UnitId -> ImportResolution -> IO ()
demandResolution demand home_uid = \case
  ResolvedHome key path     -> demand $ Summarise (mnkUnitId key) path
  ResolvedFixed key         -> demand $ Fixed key
  ResolvedExternal uid      -> demand $ UnitDeps home_uid uid
  ResolvedInstantiation iud -> demand $ Instantiate home_uid iud
  ResolvedNotFound          -> return ()

-- | The imports a module node has an edge for: the module's own imports, plus
-- its @hs-boot@ file if it has one.
--
-- The edge is dropped if the import resolves to nothing.
moduleEdgeImports :: ModSummary -> [UnresolvedImport PkgQual]
moduleEdgeImports ms =
  [ self_boot | NotBoot <- [isBootSummary ms] ] ++ map unLoc (ms_imps ms)
  where
    self_boot = (generatedImport LookupUser (ms_mod_name ms))
                  { ui_boot = IsBoot }

-- | What summarising a module must cause to be visited: everything it has an
-- edge to, plus, for an @hs-boot@ file, the source it is a boot file for.
--
-- Nothing /depends/ on that source -- if it did the graph would have a cycle,
-- which is what @hs-boot@ exists to prevent -- so it is reachable only here.
moduleDiscoveries :: ModSummary -> [UnresolvedImport PkgQual]
moduleDiscoveries ms = moduleEdgeImports ms ++ boot_source
  where
    boot_source = [ generatedImport LookupUser (ms_mod_name ms)
                  | IsBoot <- [isBootSummary ms] ]

-- | Resolve a module lookup made from the given home unit.
resolveDownsweepImport :: DownsweepEnv -> UnitId -> UnresolvedImport PkgQual -> FinderM ImportResolution
resolveDownsweepImport env home_uid lkp
  | ui_mod_name lkp `elem` ds_excl_mods env
  = pure ResolvedNotFound
  | otherwise
  = classify =<< resolveImport hsc_env lkp
  where
    -- All operations happen relative to the home unit the import was made from.
    hsc_env = hscSetActiveHomeUnit home_unit (resolveImportHscEnv env)
    home_unit = ue_unitHomeUnit home_uid (hsc_unit_env (ds_hsc_env env))

    classify :: FindResult -> FinderM ImportResolution
    classify = \case
      Found location mod
        | moduleUnitId mod `Set.member` hsc_all_home_unit_ids hsc_env
        -> home_module location mod
        | VirtUnit iud <- moduleUnit mod
        , not (isHomeModule home_unit mod)
        -> pure $ ResolvedInstantiation iud
        | otherwise
        -> pure $ ResolvedExternal (moduleUnitId mod)
      _ -> pure ResolvedNotFound
           -- Not found. If it is TRULY not found at all, we'll error when we
           -- actually try to compile.

    home_module :: ModLocation -> Module -> FinderM ImportResolution
    home_module location mod = case ds_mode env of
      DownsweepUseCompile {} ->
        pure $ case ml_hs_file_ospath location of
          Just path -> ResolvedHome key path
          Nothing   -> ResolvedNotFound
      DownsweepUseFixed ->
        -- The resulting 'Fixed' query will determine whether the
        -- interface file actually exists (see 'DownsweepUseFixed').
        pure $ ResolvedFixed key
      where
        key = moduleToMnk mod (ui_boot lkp)

-- | Summarise a home unit source file, reusing a summary that is already to
-- hand if there is one.
summariseHomeSourceFile :: DownsweepEnv -> UnitId -> OsPath -> IO SummariseResult
summariseHomeSourceFile env uid path =
  summariseSourceFile (ds_hsc_env env) home_unit opts path
  where
    home_unit = ue_unitHomeUnit uid (hsc_unit_env (ds_hsc_env env))
    opts = defaultSourceFileOptions { sfo_reuse = Map.lookup (uid, path) (ds_prior env) }

-- | Read the interface of a home module we are taking as given, and the edges
-- it records.
readFixedModule :: DownsweepEnv -> ModNodeKeyWithUid -> IO (Maybe FixedModule)
readFixedModule (DownsweepEnv { ds_hsc_env = hsc_env }) key =
  runFinderM (findExactModule hsc_env (mnkToInstalledModule key) (mnkIsBoot key)) >>= \case
    InstalledFound loc -> do
      -- MP: TODO, we should just read the dependency info from the interface rather than either
      -- a. Loading the whole thing into the EPS (this might never nececssary and causes lots of things to be permanently loaded into memory)
      -- b. Loading the whole interface into a buffer before discarding it. (wasted allocation and deserialisation)
      read_result <-
        -- 1. Check if the interface is already loaded into the EPS by some other
        -- part of the compiler.
        lookupIfaceByModuleHsc hsc_env (mnkToModule key) >>= \case
          Just iface -> return (MaybeErr.Succeeded iface)
          Nothing -> readIface (hsc_hooks hsc_env) (hsc_logger hsc_env) (hsc_dflags hsc_env) (hsc_NC hsc_env) (mnkToModule key) (ml_hi_file loc)
      return $ case read_result of
        MaybeErr.Succeeded iface -> Just (FixedModule loc (ifaceEdges (mi_deps iface)))
        -- Skip any failure, we might try to read a .hi-boot file for
        -- example, even if there is not one.
        MaybeErr.Failed {} -> Nothing
    _otherwise ->
      -- If the finder fails, just keep going, there will be another error later
      -- when we try to use this dependency.
      return Nothing
  where
    ifaceEdges :: Dependencies -> [ModuleNodeEdge]
    ifaceEdges deps =
      [ mkModuleEdge (tcImportLevel lvl) (NodeKey_Module (ModNodeKeyWithUid dep uid))
      | (lvl, uid, dep) <- Set.toList (dep_direct_mods deps)
      ] ++
      [ mkModuleEdge (tcImportLevel lvl) (NodeKey_ExternalUnit uid)
      | (lvl, uid) <- Set.toList (dep_direct_pkgs deps)
      ]

-- | The direct dependencies of an external unit, as the given home unit sees
-- them.
externalUnitDepends :: DownsweepEnv -> UnitId -> UnitId -> [UnitId]
externalUnitDepends (DownsweepEnv { ds_hsc_env = hsc_env }) home_uid uid =
  case unitDepends <$> lookupUnitId (hsc_units lcl_hsc_env) uid of
    Just deps -> deps
    Nothing   -> pprPanic "downsweep" (text "Malformed package database, missing" <+> ppr uid)
  where
    -- Set the active unit so that we find the -package flags of the home
    -- package that introduced this unit.
    lcl_hsc_env = hscSetActiveUnitId home_uid hsc_env

--------------------------------------------------------------------------------
-- * Projecting the module graph
--------------------------------------------------------------------------------

-- | The graph node an import resolved to (if any).
downsweepEdgeTarget :: DownsweepAnswers -> UnitId -> UnresolvedImport PkgQual -> Maybe NodeKey
downsweepEdgeTarget answers home_uid lkp =
  case lookupDMap (Resolve home_uid lkp) answers of
    Nothing -> Nothing
    Just (Identity resolution) -> case resolution of
      ResolvedNotFound          -> Nothing
      ResolvedExternal uid      -> Just (NodeKey_ExternalUnit uid)
      ResolvedInstantiation iud -> Just (NodeKey_Unit iud)
      ResolvedFixed key         ->
        -- Only emit an edge if the target node actually materialised
        -- (the interface file of a 'Fixed' node might turn out not to exist,
        -- e.g. due to the self-boot check).
        case lookupDMap (Fixed key) answers of
          Just (Identity (Just {}))
            -> Just (NodeKey_Module key)
          _ -> Nothing
      ResolvedHome key path     ->
        case lookupDMap (Summarise (mnkUnitId key) path) answers of
          Just (Identity (SummariseFound ms)) | msKey ms == key
            -> Just (NodeKey_Module (msKey ms))
          _ -> Nothing

-- | Build the module graph out of the final 'DownsweepAnswers'.
moduleGraphFromDownsweepAnswers
  :: Map.Map NodeKey ModuleGraphNode
     -- ^ base nodes, taken as given
  -> DownsweepAnswers
  -> ([DriverMessages], Map.Map NodeKey ModuleGraphNode)
moduleGraphFromDownsweepAnswers base answers =
  ( summarise_errors ++ Map.elems mismatch_errors ++ unit_errors
  , Map.unions
        -- Left-biased union:
      [ --   - Base nodes are taken as given: they win over everything.
        base
        --  - Freshly summarised modules must come before 'fixed_nodes',
        --    so that fresh summaries overwrite stale ones.
      , module_node_map
        --  - Then everything else (distinct keys: no possible collisions).
      , Map.fromList (fixed_nodes ++ unit_nodes ++ inst_nodes)
      ]
  )
  where
    entries :: [DSum DownsweepQuery Identity]
    entries = toListDMap answers

    summaries :: Map.Map (UnitId, OsPath) SummariseResult
    fixeds :: Map.Map ModNodeKeyWithUid (Maybe FixedModule)
    units :: Map.Map (UnitId, UnitId) [UnitId]
    insts :: [(UnitId, InstantiatedUnit)]
    summaries = Map.fromList [ ((uid, path), r) | Summarise uid path  :=> Identity r <- entries ]
    fixeds    = Map.fromList [ (    key    , r) | Fixed key           :=> Identity r <- entries ]
    units     = Map.fromList [ ((home, uid), r) | UnitDeps home uid   :=> Identity r <- entries ]
    insts     =              [  (home, iu )     | Instantiate home iu :=> _          <- entries ]

    module_nodes :: [(NodeKey, ModuleGraphNode)]
    module_nodes =
      [ ( NodeKey_Module (msKey ms), ModuleNode edges (ModuleNodeCompile ms) )
      | ((uid, _), SummariseFound ms) <- Map.toList summaries
      , let edges = [ mkModuleEdge (ui_level imp) target
                    | imp <- moduleEdgeImports ms
                    , Just target <-
                        [ downsweepEdgeTarget answers uid imp ] ]
      ]

    -- There may be duplicate nodes for a single key (e.g. duplicate root
    -- files, allowed by 'downsweep' for @ghc -M@).
    --
    -- Deduplicate by keeping the first file according to the (deterministic)
    -- 'NodeKey' order.
    module_node_map :: Map.Map NodeKey ModuleGraphNode
    module_node_map = Map.fromListWith (\ _new old -> old) module_nodes

    fixed_nodes :: [(NodeKey, ModuleGraphNode)]
    fixed_nodes =
      [ ( NodeKey_Module key, ModuleNode fixed_edges (ModuleNodeFixed key fixed_location) )
      | (key, Just (FixedModule { fixed_location, fixed_edges })) <- Map.toList fixeds ]

    unit_nodes :: [(NodeKey, ModuleGraphNode)]
    unit_nodes =
      [ ( NodeKey_ExternalUnit uid
        , UnitNode { un_home_uid = home, un_deps = deps, un_uid = uid } )
      | ((home, uid), deps) <- Map.toList units ]

    inst_nodes :: [(NodeKey, ModuleGraphNode)]
    inst_nodes =
      [ (NodeKey_Unit iud, InstantiationNode home iud) | (home, iud) <- insts ]

    summarise_errors :: [DriverMessages]
    summarise_errors =
      [ errs | (_, SummariseFailed errs) <- Map.toList summaries ]

    -- Check for a mismatch between file name and module name.
    mismatch_errors :: Map.Map (ModNodeKeyWithUid, OsPath) DriverMessages
    mismatch_errors =
      Map.fromList
        [ ( (key, path)
          , singleMessage $ mkPlainErrorMsgEnvelope (ms_mod_name_loc ms) $
              DriverFileModuleNameMismatch (ms_mod_name ms) (gwib_mod (mnkModuleName key)) )
        | Resolve _ _ :=> Identity (ResolvedHome key path) <- entries
        , Just (SummariseFound ms) <- [Map.lookup (mnkUnitId key, path) summaries]
        , msKey ms /= key
        ]

    unit_errors :: [Messages DriverMessage]
    unit_errors =
      [ singleMessage $ mkPlainErrorMsgEnvelope noSrcSpan $
          DriverInconsistentUnitDependencies uid homes
      | (uid, homes) <- Map.toList $ Map.fromListWith (flip (++))
          [ (uid, [(home, deps)]) | ((home, uid), deps) <- Map.toList units ]
      , not (allSame (map snd homes))
      ]

    allSame :: Eq a => [a] -> Bool
    allSame []       = True
    allSame (x : xs) = all (== x) xs

--------------------------------------------------------------------------------
-- * Root summaries
--------------------------------------------------------------------------------

-- | A successfully summarised root 'Target'.
data RootSummary =
  RootSummary
    { rs_file     :: !(UnitId, OsPath)
      -- ^ the summarised source file
    , rs_resolved :: !(Maybe (UnitId, UnresolvedImport PkgQual, ImportResolution))
      -- ^ for a module target, the resolution that led to the file
      --
      -- 'Nothing' for a file target
    , rs_summary  :: !ModSummary
    }

-- | The root query a summarised target contributes to the downsweep:
--
--  - a file target contributes its file to summarise
--  - a module target contributes its already-answered resolution.
rootSummaryRoots :: RootSummary -> DownsweepRoots
rootSummaryRoots ( RootSummary { rs_file, rs_resolved } ) =
  case rs_resolved of
    Nothing  -> mempty { dr_summarise   = [ rs_file ] }
    Just res -> mempty { dr_resolutions = [ res ] }

-- | Summarise one 'Target'.
getRootSummary :: DownsweepEnv -> Target -> IO (Either DriverMessages RootSummary)
getRootSummary env target =
  case targetId of
    TargetFile file mb_phase -> do
      let offset_file = augmentByWorkingDirectory dflags file
          file_not_found = singleMessage $
            mkPlainErrorMsgEnvelope noSrcSpan (DriverFileNotFound offset_file)
      exists <- doesFileExist offset_file
      if exists || isJust maybe_buf
      then summarise Nothing uid (unsafeEncodeUtf offset_file)
             defaultSourceFileOptions { sfo_start_phase = mb_phase
                                      , sfo_contents    = maybe_buf }
             file_not_found
      else return $ Left file_not_found

    TargetModule modl -> do
      let root_imp = (generatedImport LookupUser modl)
                       { ui_pkg_qual = ThisPkg (homeUnitId home_unit) }
          -- A module target has to name a home module we can compile.
          not_found = return $ Left (moduleNotFoundErr uid modl)
      resolution <- runFinderM $ resolveDownsweepImport env uid root_imp
      case resolution of
        ResolvedHome key path ->
          summarise (Just (uid, root_imp, resolution)) (mnkUnitId key) path
            defaultSourceFileOptions { sfo_contents = maybe_buf }
            (moduleNotFoundErr uid modl)
        ResolvedFixed {}         -> not_found
        ResolvedExternal {}      -> not_found
        ResolvedInstantiation {} -> not_found
        ResolvedNotFound         -> not_found
  where
      Target {targetId, targetContents = maybe_buf, targetUnitId = uid} = target
      hsc_env = ds_hsc_env env
      home_unit = ue_unitHomeUnit uid (hsc_unit_env hsc_env)
      dflags = homeUnitEnv_dflags (ue_findHomeUnitEnv uid (hsc_unit_env hsc_env))

      summarise :: Maybe (UnitId, UnresolvedImport PkgQual, ImportResolution)
                -> UnitId
                -> OsPath
                -> SourceFileOptions
                -> DriverMessages
                -> IO (Either DriverMessages RootSummary)
      summarise resolved file_uid path opts absent = do
        let file_home_unit = ue_unitHomeUnit file_uid (hsc_unit_env hsc_env)
        result <- summariseSourceFile hsc_env file_home_unit
                    opts { sfo_reuse = Map.lookup (file_uid, path) (ds_prior env) } path
        return $ case result of
          SummariseFound ms   -> Right $
            RootSummary { rs_file     = (file_uid, path)
                        , rs_resolved = resolved
                        , rs_summary  = ms }
          SummariseFailed err -> Left err
          SummariseNotThere   -> Left absent

-- | Execute 'getRootSummary' for the 'Target's in bundles, spawning one
-- worker per bundle. The number of bundles processed at once is limited by the
-- session's concurrency.
rootSummariesParallel
  :: DownsweepEnv -> [Target]
  -> IO ([DriverMessages], [RootSummary])
rootSummariesParallel env targets = do
  pool <- hscWorkerPool "root_summary_worker" hsc_env
  results <-
    mapIndependentWorkers pool
      ( \ work_env bundle ->
          mapM (getRootSummary (worker_env work_env)) bundle )
      bundles
  pure $ partitionEithers (concat results)
  where
    hsc_env = ds_hsc_env env
    worker_env work_env = env { ds_hsc_env = setHscWorkerEnv work_env hsc_env }

    bundle_size = 20

    bundles = mk_bundles targets
    mk_bundles = unfoldr \case
      [] -> Nothing
      ts -> Just (splitAt bundle_size ts)

multiRootsErr :: SourceErrorContext -> NE.NonEmpty ModSummary -> IO ()
multiRootsErr sec (summ1 NE.:| summs)
  = throwOneError sec $ fmap GhcDriverMessage $
    mkPlainErrorMsgEnvelope noSrcSpan $ DriverDuplicatedModuleDeclaration mod files
  where
    mod = ms_mod summ1
    files = mapMaybe (ml_hs_file . ms_location) (summ1:summs)

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
  -> ModuleGraph
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
  -> ModuleGraph
  -> IO ModuleGraph
enableCodeGenWhen logger tmpfs staticLife dynLife unit_env mod_graph = do
  mgMapM enable_code_gen mod_graph
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


    nodes = mgModSummaries' mod_graph

    (td_map, lookup_node) = mkStageDeps nodes

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
        | m@(ModuleNode _deps (ModuleNodeCompile ms)) <- nodes
        , isTemplateHaskellOrQQNonBoot ms
        , not (gopt Opt_UseBytecodeRatherThanObjects (ms_hspp_opts ms))
        ]

    -- The direct dependencies of modules which require byte code
    need_bc_set =
        [ (mkNodeKey m, RunStage)
        | m@(ModuleNode _deps (ModuleNodeCompile ms)) <- nodes
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
before computing the build plan, we patch the DynFlags in the ModSummary of
any home module that is imported by a module that uses Template Haskell to
generate object code.

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
-- * Pre-processing and summarising modules
-----------------------------------------------------------------------------

-- | How to summarise one source file.
data SourceFileOptions =
  SourceFileOptions
    { sfo_start_phase :: !(Maybe Phase)
      -- ^ The phase to start preprocessing at (from a @-x@ file target)
    , sfo_contents    :: !(Maybe (StringBuffer, UTCTime))
      -- ^ The file's contents, when supplied in memory rather than read from
      -- disk (from a 'Target' with 'targetContents')
    , sfo_reuse       :: !(Maybe (ModSummary, SummProvenance))
      -- ^ A summary of this file made earlier: used as it stands if it was made
      -- during this downsweep, and otherwise only while the source still hashes
      -- the same
    }

defaultSourceFileOptions :: SourceFileOptions
defaultSourceFileOptions =
  SourceFileOptions
    { sfo_start_phase = Nothing, sfo_contents = Nothing, sfo_reuse = Nothing }

-- | Summarise a home unit source file: preprocess it, parse its header, and
-- build the 'ModSummary' it describes.
--
-- Everything about the summary is a function of the file: its header says which
-- module it defines, and its extension says whether it is a source file, an
-- @hs-boot@ file or a signature. In particular the file's name need not match
-- the module it defines; that disagreement is caught where the module is
-- imported, not here.
--
-- Summarising has no effect on the 'HscEnv'; in particular, the summarised
-- module is not registered anywhere. The caller decides what to do with the
-- summary.
summariseSourceFile
  :: HscEnv
  -> HomeUnit -- ^ the home unit the file belongs to
  -> SourceFileOptions
  -> OsPath   -- ^ the source file
  -> IO SummariseResult
summariseSourceFile hsc_env' home_unit opts path =
  case sfo_reuse opts of
    -- Made during this downsweep: use it straight away.
    Just (ms, SummFresh) -> return (SummariseFound ms)
    -- Carried over from a previous one: reuse it only while the source still
    -- hashes the same.
    Just (ms, SummOld)   -> withSourceHash (checkSummaryHash hsc_env new_summary ms (ms_location ms))
    Nothing              -> withSourceHash new_summary
  where
    -- The file has to exist before there is anything to summarise. It might
    -- have been deleted since the finder found it, and this is also what a
    -- module with no hs-boot file looks like.
    withSourceHash :: (Fingerprint -> IO (Either DriverMessages ModSummary)) -> IO SummariseResult
    withSourceHash summarise = do
      mb_src_hash <- case sfo_contents opts of
        Just (buf, _) -> return $ Just $ fingerprintStringBuffer buf
        Nothing       -> fileHashIfExists src_fn
      case mb_src_hash of
        Nothing       -> return SummariseNotThere
        Just src_hash -> summarise src_hash >>= \case
          Left err -> return $ SummariseFailed err
          Right ms -> return $ SummariseFound ms

    -- Change the main active unit so that all operations happen relative to the
    -- unit the file belongs to; in particular, so that CPP is passed the right
    -- include paths. See the multiHomeUnits_cpp2 test.
    hsc_env = hscSetActiveHomeUnit home_unit hsc_env'
    src_fn = unsafeDecodeUtf path

    new_summary :: Fingerprint -> IO (Either DriverMessages ModSummary)
    new_summary src_hash = runExceptT $ do
      preimps@PreprocessedImports {..}
          <- getPreprocessedImports hsc_env src_fn (sfo_start_phase opts) (sfo_contents opts)

      let fopts = initFinderOpts (hsc_dflags hsc_env)
          (basename, extension) = splitExtension src_fn

          hsc_src
            | isHaskellSigSuffix (drop 1 extension)  = HsigFile
            | isHaskellBootSuffix (drop 1 extension) = HsBootFile
            | otherwise                              = HsSrcFile

          -- Make a ModLocation for this file, adding the @-boot@ suffix to
          -- all paths if the original was a boot file.
          location = mkHomeModLocation fopts pi_mod_name
                       (unsafeEncodeUtf basename) (unsafeEncodeUtf extension) hsc_src

      let mod = mkHomeModule home_unit pi_mod_name

      let instantiations = homeUnitInstantiations home_unit
      when (hsc_src == HsigFile && isNothing (lookup pi_mod_name instantiations)) $
          throwE $ singleMessage $ mkPlainErrorMsgEnvelope pi_mod_name_loc
                 $ DriverUnexpectedSignature pi_mod_name
                     (checkBuildingCabalPackage (hsc_dflags hsc_env)) instantiations

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
           obj_timestamp <- modificationTimeIfExists (ml_obj_file_ospath location)
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
        , ms_mod_name_loc = pi_mod_name_loc
        , ms_hsc_src = nms_hsc_src
        , ms_location = nms_location
        , ms_hspp_file = pi_hspp_fn
        , ms_hspp_opts = pi_local_dflags
        , ms_hspp_buf  = Just pi_hspp_buf
        , ms_parsed_mod = Nothing
        , ms_textual_imps =
            (noLoc . generatedImport LookupUser <$> extra_sig_imports) ++
            pi_imps
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
      , pi_imps     :: [Located (UnresolvedImport PkgQual)]
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
  (pi_imps', L pi_mod_name_loc pi_mod_name)
      <- ExceptT $ do
          mimps <- parseHeaderImports pi_local_dflags pi_hspp_buf pi_hspp_fn src_fn
          return (first (mkMessages . fmap mkDriverPsHeaderMessage . getMessages) mimps)
  let pi_imps = map (fmap (rnUnresolvedImportPkgQual (renameRawPkgQual (hsc_unit_env hsc_env)))) pi_imps'
  return PreprocessedImports {..}
