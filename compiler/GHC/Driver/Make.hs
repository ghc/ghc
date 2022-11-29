{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

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
        load, loadWithCache, load', LoadHowMuch(..), ModIfaceCache(..), noIfaceCache, newIfaceCache,
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
        ) where

import GHC.Prelude
import GHC.Platform

import GHC.Tc.Utils.Backpack

import GHC.Runtime.Interpreter
import qualified GHC.Linker.Loader as Linker


import GHC.Driver.Config.Diagnostic
import GHC.Driver.Session
import GHC.Driver.Monad
import GHC.Driver.Env
import GHC.Driver.Errors.Types
import GHC.Driver.Main


import GHC.Iface.Load      ( cannotFindModule )

import GHC.Data.Bag        ( listToBag )
import GHC.Data.Graph.Directed

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Error
import GHC.Utils.Logger

import GHC.Types.Basic
import GHC.Types.Error
import GHC.Types.SourceFile
import GHC.Types.SourceError
import GHC.Types.SrcLoc
import GHC.Types.Unique.FM

import GHC.Unit
import GHC.Unit.Env
import GHC.Unit.Finder
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Graph
import GHC.Unit.Home.ModInfo
import GHC.Unit.Module.ModDetails

import qualified Data.Set as Set

import Control.Monad
import qualified Control.Monad.Catch as MC
import Data.IORef
import System.FilePath

import GHC.Conc ( getNumProcessors )
import qualified Data.Map.Strict as M

import GHC.Driver.Make.ModIfaceCache
import GHC.Driver.Make.Upsweep
import GHC.Driver.Make.Downsweep
import GHC.Driver.Make.BuildPlan (topSortModuleGraph, createBuildPlan)

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
load how_much = loadWithCache noIfaceCache how_much

mkBatchMsg :: HscEnv -> Messager
mkBatchMsg hsc_env =
  if length (hsc_all_home_unit_ids hsc_env) > 1
    -- This also displays what unit each module is from.
    then batchMultiMsg
    else batchMsg


loadWithCache :: GhcMonad m => Maybe ModIfaceCache -> LoadHowMuch -> m SuccessFlag
loadWithCache cache how_much = do
    (errs, mod_graph) <- depanalE [] False                        -- #17459
    msg <- mkBatchMsg <$> getSession
    success <- load' cache how_much (Just msg) mod_graph
    if isEmptyMessages errs
      then pure success
      else throwErrors (fmap GhcDriverMessage errs)


-- | Generalized version of 'load' which also supports a custom
-- 'Messager' (for reporting progress) and 'ModuleGraph' (generally
-- produced by calling 'depanal'.
load' :: GhcMonad m => Maybe ModIfaceCache -> LoadHowMuch -> Maybe Messager -> ModuleGraph -> m SuccessFlag
load' mhmi_cache how_much mHscMessage mod_graph = do
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
                    liftIO $ errorMsg logger
                        (text "no such module:" <+> quotes (ppr (moduleUnit m) <> colon <> ppr (moduleName m)))
                    return Failed

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

    -- Unload everything
    liftIO $ unload interp hsc_env

    liftIO $ debugTraceMsg logger 2 (hang (text "Ready for upsweep")
                                    2 (ppr build_plan))

    n_jobs <- case parMakeCount (hsc_dflags hsc_env) of
                    Nothing -> liftIO getNumProcessors
                    Just n  -> return n

    setSession $ hscUpdateHUG (unitEnv_map pruneHomeUnitEnv) hsc_env
    (upsweep_ok, hsc_env1) <- withDeferredDiagnostics $ do
      hsc_env <- getSession
      liftIO $ upsweep n_jobs hsc_env mhmi_cache mHscMessage (toCache pruned_cache) build_plan
    setSession hsc_env1
    case upsweep_ok of
      Failed -> loadFinish upsweep_ok
      Succeeded -> do
          liftIO $ debugTraceMsg logger 2 (text "Upsweep completely successful.")
          -- Clean up after ourselves
          liftIO $ cleanCurrentModuleTempFilesMaybe logger (hsc_tmpfs hsc_env1) dflags
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
           modl = moduleName (mi_module iface)
           linkable'
                | Just ms <- lookupUFM ms_map modl
                , mi_src_hash iface /= ms_hs_hash ms
                = emptyHomeModInfoLinkable
                | otherwise
                = linkable

        ms_map = listToUFM [(ms_mod_name ms, ms) | ms <- summ]

-- ---------------------------------------------------------------------------
--
-- | Unloading
unload :: Interp -> HscEnv -> IO ()
unload interp hsc_env
  = case ghcLink (hsc_dflags hsc_env) of
        LinkInMemory -> Linker.unload interp hsc_env []
        _other -> return ()


{-
Note [--make mode]
~~~~~~~~~~~~~~~~~
There are two main parts to `--make` mode.

1. `downsweep`: Starts from the top of the module graph and computes dependencies.
2. `upsweep`: Starts from the bottom of the module graph and compiles modules.

The result of the downsweep is a 'ModuleGraph', which is then passed to 'upsweep' which
computers how to build this ModuleGraph.
-}


toCache :: [HomeModInfo] -> M.Map (ModNodeKeyWithUid) HomeModInfo
toCache hmis = M.fromList ([(miKey $ hm_iface hmi, hmi) | hmi <- hmis])

miKey :: ModIface -> ModNodeKeyWithUid
miKey hmi = ModNodeKeyWithUid (mi_mnwib hmi) ((toUnitId $ moduleUnit (mi_module hmi)))

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
              -> atomicModifyIORef' warnings $ \i -> (action: i, ())
            MCDiagnostic SevError _reason _code
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
  = mkPlainErrorMsgEnvelope loc $ GhcDriverMessage $
    DriverUnknownMessage $ UnknownDiagnostic $ mkPlainError noHints $
    cannotFindModule hsc_env wanted_mod err

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
