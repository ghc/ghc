{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module GHC.Driver.Make.Upsweep where

import GHC.Prelude

import GHC.Tc.Utils.Backpack
import GHC.Tc.Utils.Monad  ( initIfaceCheck, concatMapM )

import GHC.Linker.Types


import GHC.Driver.Config.Diagnostic
import GHC.Driver.Pipeline
import GHC.Driver.Session
import GHC.Driver.Monad
import GHC.Driver.Env
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Main


import GHC.IfaceToCore     ( typecheckIface )
import GHC.Iface.Recomp    ( RecompileRequired(..), CompileReason(..) )

import GHC.Data.Maybe      ( expectJust )

import GHC.Utils.Exception ( throwIO, SomeAsyncException )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Error
import GHC.Utils.Logger
import GHC.Utils.TmpFs

import GHC.Types.Basic
import GHC.Types.SourceFile
import GHC.Types.SourceError

import GHC.Unit
import GHC.Unit.Env
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Graph
import GHC.Unit.Home.ModInfo


import Control.Concurrent ( newQSem, waitQSem, signalQSem, ThreadId, killThread, forkIOWithUnmask )
import qualified GHC.Conc as CC
import Control.Concurrent.MVar
import Control.Monad
import qualified Control.Monad.Catch as MC
import Data.IORef
import Data.Maybe
import Data.Bifunctor (first)
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
import GHC.Types.Unique.DFM (udfmRestrictKeysSet)
import qualified Data.IntSet as I
import GHC.Driver.Make.Types
import GHC.Driver.Make.ModIfaceCache
import GHC.Driver.Make.BuildPlan
import GHC.Utils.Misc
import GHC.Driver.Make.Analysis
import GHC.Data.Graph.Directed
import GHC.Utils.Panic.Plain

{-
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


upsweep
    :: Int -- ^ The number of workers we wish to run in parallel
    -> HscEnv -- ^ The base HscEnv, which is augmented for each module
    -> Maybe ModIfaceCache -- ^ A cache to incrementally write final interface files to
    -> Maybe Messager
    -> M.Map ModNodeKeyWithUid HomeModInfo
    -> [BuildPlan]
    -> IO (SuccessFlag, HscEnv)
upsweep n_jobs hsc_env hmi_cache mHscMessage old_hpt build_plan = do
    (cycle, pipelines, collect_result) <- interpretBuildPlan (hsc_HUG hsc_env) hmi_cache old_hpt build_plan
    let !meta = strictMap make_action_meta pipelines
    runPipelines n_jobs hsc_env mHscMessage pipelines
    res <-
      if dopt Opt_D_dump_make_stats (hsc_dflags hsc_env)
        then do
          collect_result <* analyseBuildGraph (hsc_logger hsc_env) meta
        else
          collect_result

    let completed = [m | Just (Just m) <- res]
    let hsc_env' = addDepsToHscEnv completed hsc_env

    -- Handle any cycle in the original compilation graph and return the result
    -- of the upsweep.
    case cycle of
        Just mss -> do
          let logger = hsc_logger hsc_env
          liftIO $ fatalErrorMsg logger (cyclicModuleErr mss)
          return (Failed, hsc_env)
        Nothing  -> do
          let success_flag = successIf (all isJust res)
          return (success_flag, hsc_env')

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
  ((mcycle, plans), build_map) <- runStateT (buildLoop plan) (BuildLoopState M.empty 1 0 hug_var)
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
              InstantiationNode uid iu -> \(hug, deps) -> withCurrentUnit (moduleGraphNodeUnitId mod) $ do
                executeInstantiationNode mod_idx n_mods hug uid iu
                return (Nothing, deps)
              ModuleNode _build_deps ms ->
                let !old_hmi = M.lookup (msKey ms) old_hpt
                    rehydrate_mods = mapMaybe nodeKeyModName <$> rehydrate_nodes
                in \(hug, deps) -> withCurrentUnit (moduleGraphNodeUnitId mod) $ do
                       hmi <- executeCompileNode mod_idx n_mods old_hmi hug rehydrate_mods ms
                       -- Write the HMI to an external cache (if one exists)
                       -- See Note [Caching HomeModInfo]
                       liftIO $ forM_ mhmi_cache $ \hmi_cache -> addHmiToCache hmi_cache hmi
                       -- This global MVar is incrementally modified in order to avoid having to
                       -- recreate the HPT before compiling each module which leads to a quadratic amount of work.
                       liftIO $ modifyMVar_ hug_var (return . addHomeModInfoToHug hmi)
                       return (Just hmi, addToModuleNameSet (moduleGraphNodeUnitId mod) (ms_mod_name ms) deps )
              LinkNode _nks uid -> \(hug, deps) -> withCurrentUnit (moduleGraphNodeUnitId mod) $  do
                  executeLinkNode hug (mod_idx, n_mods) uid direct_deps
                  return (Nothing, deps)


      res_var <- liftIO newEmptyMVar
      let result_var = mkResultVar res_var
      make_action <- makeAction (MakeModule (mkNodeKey mod)) build_deps (wait_deps_hug hug_var) build_action res_var
      setModulePipeline (mkNodeKey mod) (mkBuildResult origin (make_action_id make_action) result_var)
      return make_action


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
      let
          !build_deps = getDependencies (map gwib_mod deps) build_map
      let loop_action (hug, tdeps) = do
            hsc_env <- asks hsc_env
            let new_hsc = setHUG hug hsc_env
                mns :: [ModuleName]
                mns = mapMaybe (nodeKeyModName . gwib_mod) deps

            hmis' <- liftIO $ rehydrateAfter new_hsc mns

            checkRehydrationInvariant hmis' deps

            -- Add hydrated interfaces to global variable
            liftIO $ modifyMVar_ hug_var (\hug -> return $ foldr addHomeModInfoToHug hug hmis')
            return (hmis', tdeps)

      action <- makeAction LoopSync build_deps (wait_deps_hug hug_var) loop_action res_var

      let fanout i = first (Just . (!! i)) <$> mkResultVar res_var
      -- From outside the module loop, anyone must wait for the loop to finish and then
      -- use the result of the rehydrated iface. This makes sure that things not in the
      -- module loop will see the updated interfaces for all the identifiers in the loop.
          boot_key :: NodeKey -> NodeKey
          boot_key (NodeKey_Module m) = NodeKey_Module (m { mnkModuleName = (mnkModuleName m) { gwib_isBoot = IsBoot } } )
          boot_key k = pprPanic "boot_key" (ppr k)

          make_id = make_action_id action

          update_module_pipeline (m, i) =
            case gwib_isBoot m of
              NotBoot -> setModulePipeline (gwib_mod m) (mkBuildResult (Loop origin) make_id (fanout i))
              IsBoot -> do
                setModulePipeline (gwib_mod m) (mkBuildResult (Loop origin) make_id (fanout i))
                -- SPECIAL: Anything outside the loop needs to see A rather than A.hs-boot
                setModulePipeline (boot_key (gwib_mod m)) (mkBuildResult (Loop origin) make_id (fanout i))

      let deps_i = zip deps [0..]
      mapM_ update_module_pipeline deps_i

      return action


      -- Checks that the interfaces returned from hydration match-up with the names of the
      -- modules which were fed into the function.
    checkRehydrationInvariant hmis deps =
        let hmi_names = map (moduleName . mi_module . hm_iface) hmis
            start = mapMaybe (nodeKeyModName . gwib_mod) deps
        in massertPpr (hmi_names == start) $ (ppr hmi_names $$ ppr start)


withCurrentUnit :: UnitId -> RunMakeM a -> RunMakeM a
withCurrentUnit uid = do
  local (\env -> env { hsc_env = hscSetActiveUnitId uid (hsc_env env)})

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
wrapAction :: HscEnv -> IO a -> IO (Maybe a)
wrapAction hsc_env k = do
  let lcl_logger = hsc_logger hsc_env
      lcl_dynflags = hsc_dflags hsc_env
      print_config = initPrintConfig lcl_dynflags
  let logg err = printMessages lcl_logger print_config (initDiagOpts lcl_dynflags) (srcErrorMessages err)
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


-- | Abstraction over the operations of a semaphore which allows usage with the
--  -j1 case
data AbstractSem = AbstractSem { acquireSem :: IO ()
                               , releaseSem :: IO () }

withAbstractSem :: (MonadIO m, MC.MonadMask m) => AbstractSem -> m b -> m b
withAbstractSem sem = MC.bracket_ (liftIO $ acquireSem sem) (liftIO $ releaseSem sem)

-- | Environment used when compiling a module
data MakeEnv = MakeEnv { hsc_env :: !HscEnv -- The basic HscEnv which will be augmented for each module
                       , compile_sem :: !AbstractSem
                       -- Modify the environment for module k, with the supplied logger modification function.
                       -- For -j1, this wrapper doesn't do anything
                       -- For -jn, the wrapper initialised a log queue and then modifies the logger to pipe its output
                       --          into the log queue.
                       , withLogger :: forall a . Int -> ((Logger -> Logger) -> IO a) -> IO a
                       , env_messager :: !(Maybe Messager)
                       }

type RunMakeM a = ReaderT MakeEnv (MaybeT IO) a

type MakeAction = MakeActionT (ReaderT MakeEnv (MaybeT IO))

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
  -- This function only does anything if the linkable produced is a BCO, which only happens with the
  -- bytecode backend, no need to guard against the backend type additionally.
  addSptEntries (hscUpdateHPT (\hpt -> addToHpt hpt (ms_mod_name summary) hmi) hsc_env)
                (homeModInfoByteCode hmi)

  return hmi

-- | Add the entries from a BCO linkable to the SPT table, see
-- See Note [Grand plan for static forms] in GHC.Iface.Tidy.StaticPtrTable.
addSptEntries :: HscEnv -> Maybe Linkable -> IO ()
addSptEntries hsc_env mlinkable =
  hscAddSptEntries hsc_env
     [ spt
     | Just linkable <- [mlinkable]
     , unlinked <- linkableUnlinked linkable
     , BCOs _ spts <- pure unlinked
     , spt <- spts
     ]


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
        lift $ MaybeT $ withLoggerHsc k env $ \hsc_env ->
          let lcl_hsc_env = setHUG deps hsc_env
          in wrapAction lcl_hsc_env $ do
            res <- upsweep_inst lcl_hsc_env msg k n uid iu
            cleanCurrentModuleTempFilesMaybe (hsc_logger hsc_env) (hsc_tmpfs hsc_env) (hsc_dflags hsc_env)
            return res


cleanCurrentModuleTempFilesMaybe :: MonadIO m => Logger -> TmpFs -> DynFlags -> m ()
cleanCurrentModuleTempFilesMaybe logger tmpfs dflags =
  unless (gopt Opt_KeepTmpFiles dflags) $
    liftIO $ cleanCurrentModuleTempFiles logger tmpfs


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
  lift $ MaybeT (withLoggerHsc k me $ \hsc_env -> do
     hydrated_hsc_env <- liftIO $ maybeRehydrateBefore (setHUG hug hsc_env) mod fixed_mrehydrate_mods
     let -- Use the cached DynFlags which includes OPTIONS_GHC pragmas
         lcl_dynflags = ms_hspp_opts mod
     let lcl_hsc_env =
             -- Localise the hsc_env to use the cached flags
             hscSetFlags lcl_dynflags $
             hydrated_hsc_env
     -- Compile the module, locking with a semaphore to avoid too many modules
     -- being compiled at the same time leading to high memory usage.
     wrapAction lcl_hsc_env $ do
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
type uses the AbstractTyCon T; and a TyCon for `S` that also mentions that same
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

    linkresult <- liftIO $
                            link (ghcLink dflags)
                                (hsc_logger hsc_env')
                                (hsc_tmpfs hsc_env')
                                (hsc_hooks hsc_env')
                                dflags
                                (hsc_unit_env hsc_env')
                                True -- We already decided to link
                                msg'
                                (hsc_HPT hsc_env')
    case linkresult of
      Failed -> fail "Link Failed"
      Succeeded -> return ()


-- | Wait for some dependencies to finish and then read from the given MVar.
wait_deps_hug :: MVar HomeUnitGraph -> [BuildResult] -> ReaderT MakeEnv (MaybeT IO) (HomeUnitGraph, ModuleNameSet)
wait_deps_hug hug_var deps = do
  (_, module_deps) <- wait_deps deps
  hug <- liftIO $ readMVar hug_var
  let pruneHomeUnitEnv uid hme =
        let -- Restrict to things which are in the transitive closure to avoid retaining
            -- reference to loop modules which have already been compiled by other threads.
            -- See Note [ModuleNameSet, efficiency and space leaks]
            !new = udfmRestrictKeysSet (homeUnitEnv_hpt hme) (fromMaybe I.empty $ M.lookup uid module_deps)
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
    unionModuleNameSet = M.unionWith I.union


-- Executing the pipelines

label_self :: String -> IO ()
label_self thread_name = do
    self_tid <- CC.myThreadId
    CC.labelThread self_tid thread_name

runPipelines :: Int -> HscEnv -> Maybe Messager -> [MakeAction] -> IO ()
-- Don't even initialise plugins if there are no pipelines
runPipelines _ _ _ [] = return ()
runPipelines n_job orig_hsc_env mHscMessager all_pipelines = do
  liftIO $ label_self "main --make thread"

  plugins_hsc_env <- initializePlugins orig_hsc_env
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
  wait_log_thread <- logThread n_jobs (length all_pipelines) (hsc_logger plugin_hsc_env) stopped_var log_queue_queue_var


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

-- | Execute each action in order, limiting the amount of parallelism by the given
-- semaphore.
runLoop :: (((forall a. IO a -> IO a) -> IO ()) -> IO a) -> MakeEnv -> [MakeAction] -> IO [a]
runLoop _ _env [] = return []
runLoop fork_thread env (ma:acts) = do
  new_thread <- forkMakeAction fork_thread env ma
  threads <- runLoop fork_thread env acts
  return (new_thread : threads)


forkMakeAction :: (((forall a. IO a -> IO a) -> IO ()) -> IO a) -> MakeEnv -> MakeAction -> IO a
forkMakeAction fork_thread env (MakeAction _deps wait act res_var (MakeActionMeta action_name id _ timing_var)) =
    fork_thread $ \unmask -> (do
            mres <- (unmask $ run_pipeline (withLocalTmpFS $ do
                        wait >>= withAbstractSem (compile_sem env)
                                 . with_timing . act

                    ))
                      `MC.onException` (putMVar res_var Nothing) -- Defensive: If there's an unhandled exception then still signal the failure.
            putMVar res_var mres)
  where
      run_pipeline :: RunMakeM a -> IO (Maybe a)
      run_pipeline p = runMaybeT (runReaderT p env)

      with_timing = withTimingSilentX (hsc_logger $ hsc_env env)
                            Opt_D_dump_make_stats
                            action_herald
                            (const ())
                            write_timing_result

      action_herald =  text "MAKE:" <> ppr id <+> ppr action_name
      write_timing_result = liftIO . writeIORef (getIORefMaybe timing_var) . Just


waitMakeAction :: MakeAction -> IO ()
waitMakeAction (MakeAction{make_res_var}) = () <$ readMVar make_res_var

cyclicModuleErr :: [ModuleGraphNode] -> SDoc
-- From a strongly connected component we find
-- a single cycle to report
cyclicModuleErr mss
  = assert (not (null mss)) $
    case findCycle graph of
       Nothing   -> text "Unexpected non-cycle" <+> ppr mss
       Just path0 -> vcat
        [ text "Module graph contains a cycle:"
        , nest 2 (show_path path0)]
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
    ppr_node (ModuleNode _deps m) = text "module" <+> ppr_ms m
    ppr_node (InstantiationNode _uid u) = text "instantiated unit" <+> ppr u
    ppr_node (LinkNode uid _) = pprPanic "LinkNode should not be in a cycle" (ppr uid)

    ppr_ms :: ModSummary -> SDoc
    ppr_ms ms = quotes (ppr (moduleName (ms_mod ms))) <+>
                (parens (text (msHsFilePath ms)))