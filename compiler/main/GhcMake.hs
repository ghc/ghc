{-# LANGUAGE BangPatterns, CPP, NondecreasingIndentation, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2011
--
-- This module implements multi-module compilation, and is used
-- by --make and GHCi.
--
-- -----------------------------------------------------------------------------
module GhcMake(
        depanal, depanalPartial,
        load, load', LoadHowMuch(..),

        downsweep,

        topSortModuleGraph,

        ms_home_srcimps, ms_home_imps,

        IsBoot(..),
        summariseModule,
        hscSourceToIsBoot,
        findExtraSigImports,
        implicitRequirements,

        noModError, cyclicModuleErr,
        moduleGraphNodes, SummaryNode
    ) where

#include "HsVersions.h"

import GhcPrelude

import qualified Linker         ( unload )

import DriverPhases
import DriverPipeline
import DynFlags
import ErrUtils
import Finder
import GhcMonad
import HeaderInfo
import HscTypes
import Module
import GHC.IfaceToCore  ( typecheckIface )
import TcRnMonad        ( initIfaceCheck )
import HscMain

import Bag              ( unitBag, listToBag, unionManyBags, isEmptyBag )
import BasicTypes
import Digraph
import Exception        ( tryIO, gbracket, gfinally )
import FastString
import Maybes           ( expectJust )
import Name
import MonadUtils       ( allM, MonadIO )
import Outputable
import Panic
import SrcLoc
import StringBuffer
import UniqFM
import UniqDSet
import TcBackpack
import Packages
import UniqSet
import Util
import qualified GHC.LanguageExtensions as LangExt
import NameEnv
import FileCleanup

import Data.Either ( rights, partitionEithers )
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import qualified FiniteMap as Map ( insertListWith )

import Control.Concurrent ( forkIOWithUnmask, killThread )
import qualified GHC.Conc as CC
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except ( ExceptT(..), runExceptT, throwE )
import Data.IORef
import Data.List
import qualified Data.List as List
import Data.Foldable (toList)
import Data.Maybe
import Data.Ord ( comparing )
import Data.Time
import System.Directory
import System.FilePath
import System.IO        ( fixIO )
import System.IO.Error  ( isDoesNotExistError )

import GHC.Conc ( getNumProcessors, getNumCapabilities, setNumCapabilities )

label_self :: String -> IO ()
label_self thread_name = do
    self_tid <- CC.myThreadId
    CC.labelThread self_tid thread_name

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
--
depanal :: GhcMonad m =>
           [ModuleName]  -- ^ excluded modules
        -> Bool          -- ^ allow duplicate roots
        -> m ModuleGraph
depanal excluded_mods allow_dup_roots = do
    hsc_env <- getSession
    (errs, mod_graph) <- depanalPartial excluded_mods allow_dup_roots
    if isEmptyBag errs
      then do
        warnMissingHomeModules hsc_env mod_graph
        setSession hsc_env { hsc_mod_graph = mod_graph }
        return mod_graph
      else throwErrors errs


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
    -> m (ErrorMessages, ModuleGraph)
    -- ^ possibly empty 'Bag' of errors and a module graph.
depanalPartial excluded_mods allow_dup_roots = do
  hsc_env <- getSession
  let
         dflags  = hsc_dflags hsc_env
         targets = hsc_targets hsc_env
         old_graph = hsc_mod_graph hsc_env

  withTiming dflags (text "Chasing dependencies") (const ()) $ do
    liftIO $ debugTraceMsg dflags 2 (hcat [
              text "Chasing modules from: ",
              hcat (punctuate comma (map pprTarget targets))])

    -- Home package modules may have been moved or deleted, and new
    -- source files may have appeared in the home package that shadow
    -- external package modules, so we have to discard the existing
    -- cached finder data.
    liftIO $ flushFinderCaches hsc_env

    mod_summariesE <- liftIO $ downsweep hsc_env (mgModSummaries old_graph)
                                     excluded_mods allow_dup_roots
    let
           (errs, mod_summaries) = partitionEithers mod_summariesE
           mod_graph = mkModuleGraph mod_summaries
    return (unionManyBags errs, mod_graph)

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
warnMissingHomeModules :: GhcMonad m => HscEnv -> ModuleGraph -> m ()
warnMissingHomeModules hsc_env mod_graph =
    when (wopt Opt_WarnMissingHomeModules dflags && not (null missing)) $
        logWarnings (listToBag [warn])
  where
    dflags = hsc_dflags hsc_env
    targets = map targetId (hsc_targets hsc_env)

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

    msg
      | gopt Opt_BuildingCabalPackage dflags
      = hang
          (text "These modules are needed for compilation but not listed in your .cabal file's other-modules: ")
          4
          (sep (map ppr missing))
      | otherwise
      =
        hang
          (text "Modules are not listed in command line but needed for compilation: ")
          4
          (sep (map ppr missing))
    warn = makeIntoWarning
      (Reason Opt_WarnMissingHomeModules)
      (mkPlainErrMsg dflags noSrcSpan msg)

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
-- possible.  Depending on the target (see 'DynFlags.hscTarget') compiling
-- and loading may result in files being created on disk.
--
-- Calls the 'defaultWarnErrLogger' after each compiling each module, whether
-- successful or not.
--
-- Throw a 'SourceError' if errors are encountered before the actual
-- compilation starts (e.g., during dependency analysis).  All other errors
-- are reported using the 'defaultWarnErrLogger'.
--
load :: GhcMonad m => LoadHowMuch -> m SuccessFlag
load how_much = do
    mod_graph <- depanal [] False
    success <- load' how_much (Just batchMsg) mod_graph
    warnUnusedPackages
    pure success

-- Note [Unused packages]
--
-- Cabal passes `--package-id` flag for each direct dependency. But GHC
-- loads them lazily, so when compilation is done, we have a list of all
-- actually loaded packages. All the packages, specified on command line,
-- but never loaded, are probably unused dependencies.

warnUnusedPackages :: GhcMonad m => m ()
warnUnusedPackages = do
    hsc_env <- getSession
    eps <- liftIO $ hscEPS hsc_env

    let dflags = hsc_dflags hsc_env
        pit = eps_PIT eps

    let loadedPackages
          = map (getPackageDetails dflags)
          . nub . sort
          . map moduleUnitId
          . moduleEnvKeys
          $ pit

        requestedArgs = mapMaybe packageArg (packageFlags dflags)

        unusedArgs
          = filter (\arg -> not $ any (matching dflags arg) loadedPackages)
                   requestedArgs

    let warn = makeIntoWarning
          (Reason Opt_WarnUnusedPackages)
          (mkPlainErrMsg dflags noSrcSpan msg)
        msg = vcat [ text "The following packages were specified" <+>
                     text "via -package or -package-id flags,"
                   , text "but were not needed for compilation:"
                   , nest 2 (vcat (map (withDash . pprUnusedArg) unusedArgs)) ]

    when (wopt Opt_WarnUnusedPackages dflags && not (null unusedArgs)) $
      logWarnings (listToBag [warn])

    where
        packageArg (ExposePackage _ arg _) = Just arg
        packageArg _ = Nothing

        pprUnusedArg (PackageArg str) = text str
        pprUnusedArg (UnitIdArg uid) = ppr uid

        withDash = (<+>) (text "-")

        matchingStr :: String -> UnitInfo -> Bool
        matchingStr str p
                =  str == sourcePackageIdString p
                || str == packageNameString p

        matching :: DynFlags -> PackageArg -> UnitInfo -> Bool
        matching _ (PackageArg str) p = matchingStr str p
        matching dflags (UnitIdArg uid) p = uid == realUnitId dflags p

        -- For wired-in packages, we have to unwire their id,
        -- otherwise they won't match package flags
        realUnitId :: DynFlags -> UnitInfo -> UnitId
        realUnitId dflags
          = unwireUnitId dflags
          . DefiniteUnitId
          . DefUnitId
          . installedUnitInfoId

-- | Generalized version of 'load' which also supports a custom
-- 'Messager' (for reporting progress) and 'ModuleGraph' (generally
-- produced by calling 'depanal'.
load' :: GhcMonad m => LoadHowMuch -> Maybe Messager -> ModuleGraph -> m SuccessFlag
load' how_much mHscMessage mod_graph = do
    modifySession $ \hsc_env -> hsc_env { hsc_mod_graph = mod_graph }
    guessOutputFile
    hsc_env <- getSession

    let hpt1   = hsc_HPT hsc_env
    let dflags = hsc_dflags hsc_env

    -- The "bad" boot modules are the ones for which we have
    -- B.hs-boot in the module graph, but no B.hs
    -- The downsweep should have ensured this does not happen
    -- (see msDeps)
    let all_home_mods =
          mkUniqSet [ ms_mod_name s
                    | s <- mgModSummaries mod_graph, not (isBootSummary s)]
    -- TODO: Figure out what the correct form of this assert is. It's violated
    -- when you have HsBootMerge nodes in the graph: then you'll have hs-boot
    -- files without corresponding hs files.
    --  bad_boot_mods = [s        | s <- mod_graph, isBootSummary s,
    --                              not (ms_mod_name s `elem` all_home_mods)]
    -- ASSERT( null bad_boot_mods ) return ()

    -- check that the module given in HowMuch actually exists, otherwise
    -- topSortModuleGraph will bomb later.
    let checkHowMuch (LoadUpTo m)           = checkMod m
        checkHowMuch (LoadDependenciesOf m) = checkMod m
        checkHowMuch _ = id

        checkMod m and_then
            | m `elementOfUniqSet` all_home_mods = and_then
            | otherwise = do
                    liftIO $ errorMsg dflags (text "no such module:" <+>
                                     quotes (ppr m))
                    return Failed

    checkHowMuch how_much $ do

    -- mg2_with_srcimps drops the hi-boot nodes, returning a
    -- graph with cycles.  Among other things, it is used for
    -- backing out partially complete cycles following a failed
    -- upsweep, and for removing from hpt all the modules
    -- not in strict downwards closure, during calls to compile.
    let mg2_with_srcimps :: [SCC ModSummary]
        mg2_with_srcimps = topSortModuleGraph True mod_graph Nothing

    -- If we can determine that any of the {-# SOURCE #-} imports
    -- are definitely unnecessary, then emit a warning.
    warnUnnecessarySourceImports mg2_with_srcimps

    let
        -- check the stability property for each module.
        stable_mods@(stable_obj,stable_bco)
            = checkStability hpt1 mg2_with_srcimps all_home_mods

        -- prune bits of the HPT which are definitely redundant now,
        -- to save space.
        pruned_hpt = pruneHomePackageTable hpt1
                            (flattenSCCs mg2_with_srcimps)
                            stable_mods

    _ <- liftIO $ evaluate pruned_hpt

    -- before we unload anything, make sure we don't leave an old
    -- interactive context around pointing to dead bindings.  Also,
    -- write the pruned HPT to allow the old HPT to be GC'd.
    setSession $ discardIC $ hsc_env { hsc_HPT = pruned_hpt }

    liftIO $ debugTraceMsg dflags 2 (text "Stable obj:" <+> ppr stable_obj $$
                            text "Stable BCO:" <+> ppr stable_bco)

    -- Unload any modules which are going to be re-linked this time around.
    let stable_linkables = [ linkable
                           | m <- nonDetEltsUniqSet stable_obj ++
                                  nonDetEltsUniqSet stable_bco,
                             -- It's OK to use nonDetEltsUniqSet here
                             -- because it only affects linking. Besides
                             -- this list only serves as a poor man's set.
                             Just hmi <- [lookupHpt pruned_hpt m],
                             Just linkable <- [hm_linkable hmi] ]
    liftIO $ unload hsc_env stable_linkables

    -- We could at this point detect cycles which aren't broken by
    -- a source-import, and complain immediately, but it seems better
    -- to let upsweep_mods do this, so at least some useful work gets
    -- done before the upsweep is abandoned.
    --hPutStrLn stderr "after tsort:\n"
    --hPutStrLn stderr (showSDoc (vcat (map ppr mg2)))

    -- Now do the upsweep, calling compile for each module in
    -- turn.  Final result is version 3 of everything.

    -- Topologically sort the module graph, this time including hi-boot
    -- nodes, and possibly just including the portion of the graph
    -- reachable from the module specified in the 2nd argument to load.
    -- This graph should be cycle-free.
    -- If we're restricting the upsweep to a portion of the graph, we
    -- also want to retain everything that is still stable.
    let full_mg :: [SCC ModSummary]
        full_mg    = topSortModuleGraph False mod_graph Nothing

        maybe_top_mod = case how_much of
                            LoadUpTo m           -> Just m
                            LoadDependenciesOf m -> Just m
                            _                    -> Nothing

        partial_mg0 :: [SCC ModSummary]
        partial_mg0 = topSortModuleGraph False mod_graph maybe_top_mod

        -- LoadDependenciesOf m: we want the upsweep to stop just
        -- short of the specified module (unless the specified module
        -- is stable).
        partial_mg
            | LoadDependenciesOf _mod <- how_much
            = ASSERT( case last partial_mg0 of
                        AcyclicSCC ms -> ms_mod_name ms == _mod; _ -> False )
              List.init partial_mg0
            | otherwise
            = partial_mg0

        stable_mg =
            [ AcyclicSCC ms
            | AcyclicSCC ms <- full_mg,
              stable_mod_summary ms ]

        stable_mod_summary ms =
          ms_mod_name ms `elementOfUniqSet` stable_obj ||
          ms_mod_name ms `elementOfUniqSet` stable_bco

        -- the modules from partial_mg that are not also stable
        -- NB. also keep cycles, we need to emit an error message later
        unstable_mg = filter not_stable partial_mg
          where not_stable (CyclicSCC _) = True
                not_stable (AcyclicSCC ms)
                   = not $ stable_mod_summary ms

        -- Load all the stable modules first, before attempting to load
        -- an unstable module (#7231).
        mg = stable_mg ++ unstable_mg

    -- clean up between compilations
    let cleanup = cleanCurrentModuleTempFiles . hsc_dflags
    liftIO $ debugTraceMsg dflags 2 (hang (text "Ready for upsweep")
                               2 (ppr mg))

    n_jobs <- case parMakeCount dflags of
                    Nothing -> liftIO getNumProcessors
                    Just n  -> return n
    let upsweep_fn | n_jobs > 1 = parUpsweep n_jobs
                   | otherwise  = upsweep

    setSession hsc_env{ hsc_HPT = emptyHomePackageTable }
    (upsweep_ok, modsUpswept) <- withDeferredDiagnostics $
      upsweep_fn mHscMessage pruned_hpt stable_mods cleanup mg

    -- Make modsDone be the summaries for each home module now
    -- available; this should equal the domain of hpt3.
    -- Get in in a roughly top .. bottom order (hence reverse).

    let modsDone = reverse modsUpswept

    -- Try and do linking in some form, depending on whether the
    -- upsweep was completely or only partially successful.

    if succeeded upsweep_ok

     then
       -- Easy; just relink it all.
       do liftIO $ debugTraceMsg dflags 2 (text "Upsweep completely successful.")

          -- Clean up after ourselves
          hsc_env1 <- getSession
          liftIO $ cleanCurrentModuleTempFiles dflags

          -- Issue a warning for the confusing case where the user
          -- said '-o foo' but we're not going to do any linking.
          -- We attempt linking if either (a) one of the modules is
          -- called Main, or (b) the user said -no-hs-main, indicating
          -- that main() is going to come from somewhere else.
          --
          let ofile = outputFile dflags
          let no_hs_main = gopt Opt_NoHsMain dflags
          let
            main_mod = mainModIs dflags
            a_root_is_Main = mgElemModule mod_graph main_mod
            do_linking = a_root_is_Main || no_hs_main || ghcLink dflags == LinkDynLib || ghcLink dflags == LinkStaticLib

          -- link everything together
          linkresult <- liftIO $ link (ghcLink dflags) dflags do_linking (hsc_HPT hsc_env1)

          if ghcLink dflags == LinkBinary && isJust ofile && not do_linking
             then do
                liftIO $ errorMsg dflags $ text
                   ("output was redirected with -o, " ++
                    "but no output will be generated\n" ++
                    "because there is no " ++
                    moduleNameString (moduleName main_mod) ++ " module.")
                -- This should be an error, not a warning (#10895).
                loadFinish Failed linkresult
             else
                loadFinish Succeeded linkresult

     else
       -- Tricky.  We need to back out the effects of compiling any
       -- half-done cycles, both so as to clean up the top level envs
       -- and to avoid telling the interactive linker to link them.
       do liftIO $ debugTraceMsg dflags 2 (text "Upsweep partially successful.")

          let modsDone_names
                 = map ms_mod modsDone
          let mods_to_zap_names
                 = findPartiallyCompletedCycles modsDone_names
                      mg2_with_srcimps
          let (mods_to_clean, mods_to_keep) =
                partition ((`Set.member` mods_to_zap_names).ms_mod) modsDone
          hsc_env1 <- getSession
          let hpt4 = hsc_HPT hsc_env1
              -- We must change the lifetime to TFL_CurrentModule for any temp
              -- file created for an element of mod_to_clean during the upsweep.
              -- These include preprocessed files and object files for loaded
              -- modules.
              unneeded_temps = concat
                [ms_hspp_file : object_files
                | ModSummary{ms_mod, ms_hspp_file} <- mods_to_clean
                , let object_files = maybe [] linkableObjs $
                        lookupHpt hpt4 (moduleName ms_mod)
                        >>= hm_linkable
                ]
          liftIO $
            changeTempFilesLifetime dflags TFL_CurrentModule unneeded_temps
          liftIO $ cleanCurrentModuleTempFiles dflags

          let hpt5 = retainInTopLevelEnvs (map ms_mod_name mods_to_keep)
                                          hpt4

          -- Clean up after ourselves

          -- there should be no Nothings where linkables should be, now
          let just_linkables =
                    isNoLink (ghcLink dflags)
                 || allHpt (isJust.hm_linkable)
                        (filterHpt ((== HsSrcFile).mi_hsc_src.hm_iface)
                                hpt5)
          ASSERT( just_linkables ) do

          -- Link everything together
          linkresult <- liftIO $ link (ghcLink dflags) dflags False hpt5

          modifySession $ \hsc_env -> hsc_env{ hsc_HPT = hpt5 }
          loadFinish Failed linkresult


-- | Finish up after a load.
loadFinish :: GhcMonad m => SuccessFlag -> SuccessFlag -> m SuccessFlag

-- If the link failed, unload everything and return.
loadFinish _all_ok Failed
  = do hsc_env <- getSession
       liftIO $ unload hsc_env []
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
  = discardIC $ hsc_env { hsc_mod_graph = emptyMG
                        , hsc_HPT = emptyHomePackageTable }

-- | Discard the contents of the InteractiveContext, but keep the DynFlags.
-- It will also keep ic_int_print and ic_monad if their names are from
-- external packages.
discardIC :: HscEnv -> HscEnv
discardIC hsc_env
  = hsc_env { hsc_IC = empty_ic { ic_int_print = new_ic_int_print
                                , ic_monad = new_ic_monad } }
  where
  -- Force the new values for ic_int_print and ic_monad to avoid leaking old_ic
  !new_ic_int_print = keep_external_name ic_int_print
  !new_ic_monad = keep_external_name ic_monad
  dflags = ic_dflags old_ic
  old_ic = hsc_IC hsc_env
  empty_ic = emptyInteractiveContext dflags
  keep_external_name ic_name
    | nameIsFromExternalPackage this_pkg old_name = old_name
    | otherwise = ic_name empty_ic
    where
    this_pkg = thisPackage dflags
    old_name = ic_name old_ic

-- | If there is no -o option, guess the name of target executable
-- by using top-level source file name as a base.
guessOutputFile :: GhcMonad m => m ()
guessOutputFile = modifySession $ \env ->
    let dflags = hsc_dflags env
        -- Force mod_graph to avoid leaking env
        !mod_graph = hsc_mod_graph env
        mainModuleSrcPath :: Maybe String
        mainModuleSrcPath = do
            ms <- mgLookupModule mod_graph (mainModIs dflags)
            ml_hs_file (ms_location ms)
        name = fmap dropExtension mainModuleSrcPath

        name_exe = do
#if defined(mingw32_HOST_OS)
          -- we must add the .exe extension unconditionally here, otherwise
          -- when name has an extension of its own, the .exe extension will
          -- not be added by DriverPipeline.exeFileName.  See #2248
          name' <- fmap (<.> "exe") name
#else
          name' <- name
#endif
          mainModuleSrcPath' <- mainModuleSrcPath
          -- #9930: don't clobber input files (unless they ask for it)
          if name' == mainModuleSrcPath'
            then throwGhcException . UsageError $
                 "default output name would overwrite the input file; " ++
                 "must specify -o explicitly"
            else Just name'
    in
    case outputFile dflags of
        Just _ -> env
        Nothing -> env { hsc_dflags = dflags { outputFile = name_exe } }

-- -----------------------------------------------------------------------------
--
-- | Prune the HomePackageTable
--
-- Before doing an upsweep, we can throw away:
--
--   - For non-stable modules:
--      - all ModDetails, all linked code
--   - all unlinked code that is out of date with respect to
--     the source file
--
-- This is VERY IMPORTANT otherwise we'll end up requiring 2x the
-- space at the end of the upsweep, because the topmost ModDetails of the
-- old HPT holds on to the entire type environment from the previous
-- compilation.
pruneHomePackageTable :: HomePackageTable
                      -> [ModSummary]
                      -> StableModules
                      -> HomePackageTable
pruneHomePackageTable hpt summ (stable_obj, stable_bco)
  = mapHpt prune hpt
  where prune hmi
          | is_stable modl = hmi'
          | otherwise      = hmi'{ hm_details = emptyModDetails }
          where
           modl = moduleName (mi_module (hm_iface hmi))
           hmi' | Just l <- hm_linkable hmi, linkableTime l < ms_hs_date ms
                = hmi{ hm_linkable = Nothing }
                | otherwise
                = hmi
                where ms = expectJust "prune" (lookupUFM ms_map modl)

        ms_map = listToUFM [(ms_mod_name ms, ms) | ms <- summ]

        is_stable m =
          m `elementOfUniqSet` stable_obj ||
          m `elementOfUniqSet` stable_bco

-- -----------------------------------------------------------------------------
--
-- | Return (names of) all those in modsDone who are part of a cycle as defined
-- by theGraph.
findPartiallyCompletedCycles :: [Module] -> [SCC ModSummary] -> Set.Set Module
findPartiallyCompletedCycles modsDone theGraph
   = Set.unions
       [mods_in_this_cycle
       | CyclicSCC vs <- theGraph  -- Acyclic? Not interesting.
       , let names_in_this_cycle = Set.fromList (map ms_mod vs)
             mods_in_this_cycle =
                    Set.intersection (Set.fromList modsDone) names_in_this_cycle
         -- If size mods_in_this_cycle == size names_in_this_cycle,
         -- then this cycle has already been completed and we're not
         -- interested.
       , Set.size mods_in_this_cycle < Set.size names_in_this_cycle]


-- ---------------------------------------------------------------------------
--
-- | Unloading
unload :: HscEnv -> [Linkable] -> IO ()
unload hsc_env stable_linkables -- Unload everything *except* 'stable_linkables'
  = case ghcLink (hsc_dflags hsc_env) of
        LinkInMemory -> Linker.unload hsc_env stable_linkables
        _other -> return ()

-- -----------------------------------------------------------------------------
{- |

  Stability tells us which modules definitely do not need to be recompiled.
  There are two main reasons for having stability:

   - avoid doing a complete upsweep of the module graph in GHCi when
     modules near the bottom of the tree have not changed.

   - to tell GHCi when it can load object code: we can only load object code
     for a module when we also load object code fo  all of the imports of the
     module.  So we need to know that we will definitely not be recompiling
     any of these modules, and we can use the object code.

  The stability check is as follows.  Both stableObject and
  stableBCO are used during the upsweep phase later.

@
  stable m = stableObject m || stableBCO m

  stableObject m =
        all stableObject (imports m)
        && old linkable does not exist, or is == on-disk .o
        && date(on-disk .o) > date(.hs)

  stableBCO m =
        all stable (imports m)
        && date(BCO) > date(.hs)
@

  These properties embody the following ideas:

    - if a module is stable, then:

        - if it has been compiled in a previous pass (present in HPT)
          then it does not need to be compiled or re-linked.

        - if it has not been compiled in a previous pass,
          then we only need to read its .hi file from disk and
          link it to produce a 'ModDetails'.

    - if a modules is not stable, we will definitely be at least
      re-linking, and possibly re-compiling it during the 'upsweep'.
      All non-stable modules can (and should) therefore be unlinked
      before the 'upsweep'.

    - Note that objects are only considered stable if they only depend
      on other objects.  We can't link object code against byte code.

    - Note that even if an object is stable, we may end up recompiling
      if the interface is out of date because an *external* interface
      has changed.  The current code in GhcMake handles this case
      fairly poorly, so be careful.
-}

type StableModules =
  ( UniqSet ModuleName  -- stableObject
  , UniqSet ModuleName  -- stableBCO
  )


checkStability
        :: HomePackageTable   -- HPT from last compilation
        -> [SCC ModSummary]   -- current module graph (cyclic)
        -> UniqSet ModuleName -- all home modules
        -> StableModules

checkStability hpt sccs all_home_mods =
  foldl' checkSCC (emptyUniqSet, emptyUniqSet) sccs
  where
   checkSCC :: StableModules -> SCC ModSummary -> StableModules
   checkSCC (stable_obj, stable_bco) scc0
     | stableObjects = (addListToUniqSet stable_obj scc_mods, stable_bco)
     | stableBCOs    = (stable_obj, addListToUniqSet stable_bco scc_mods)
     | otherwise     = (stable_obj, stable_bco)
     where
        scc = flattenSCC scc0
        scc_mods = map ms_mod_name scc
        home_module m =
          m `elementOfUniqSet` all_home_mods && m `notElem` scc_mods

        scc_allimps = nub (filter home_module (concatMap ms_home_allimps scc))
            -- all imports outside the current SCC, but in the home pkg

        stable_obj_imps = map (`elementOfUniqSet` stable_obj) scc_allimps
        stable_bco_imps = map (`elementOfUniqSet` stable_bco) scc_allimps

        stableObjects =
           and stable_obj_imps
           && all object_ok scc

        stableBCOs =
           and (zipWith (||) stable_obj_imps stable_bco_imps)
           && all bco_ok scc

        object_ok ms
          | gopt Opt_ForceRecomp (ms_hspp_opts ms) = False
          | Just t <- ms_obj_date ms  =  t >= ms_hs_date ms
                                         && same_as_prev t
          | otherwise = False
          where
             same_as_prev t = case lookupHpt hpt (ms_mod_name ms) of
                                Just hmi  | Just l <- hm_linkable hmi
                                 -> isObjectLinkable l && t == linkableTime l
                                _other  -> True
                -- why '>=' rather than '>' above?  If the filesystem stores
                -- times to the nearest second, we may occasionally find that
                -- the object & source have the same modification time,
                -- especially if the source was automatically generated
                -- and compiled.  Using >= is slightly unsafe, but it matches
                -- make's behaviour.
                --
                -- But see #5527, where someone ran into this and it caused
                -- a problem.

        bco_ok ms
          | gopt Opt_ForceRecomp (ms_hspp_opts ms) = False
          | otherwise = case lookupHpt hpt (ms_mod_name ms) of
                Just hmi  | Just l <- hm_linkable hmi ->
                        not (isObjectLinkable l) &&
                        linkableTime l >= ms_hs_date ms
                _other  -> False

{- Parallel Upsweep
 -
 - The parallel upsweep attempts to concurrently compile the modules in the
 - compilation graph using multiple Haskell threads.
 -
 - The Algorithm
 -
 - A Haskell thread is spawned for each module in the module graph, waiting for
 - its direct dependencies to finish building before it itself begins to build.
 -
 - Each module is associated with an initially empty MVar that stores the
 - result of that particular module's compile. If the compile succeeded, then
 - the HscEnv (synchronized by an MVar) is updated with the fresh HMI of that
 - module, and the module's HMI is deleted from the old HPT (synchronized by an
 - IORef) to save space.
 -
 - Instead of immediately outputting messages to the standard handles, all
 - compilation output is deferred to a per-module TQueue. A QSem is used to
 - limit the number of workers that are compiling simultaneously.
 -
 - Meanwhile, the main thread sequentially loops over all the modules in the
 - module graph, outputting the messages stored in each module's TQueue.
-}

-- | Each module is given a unique 'LogQueue' to redirect compilation messages
-- to. A 'Nothing' value contains the result of compilation, and denotes the
-- end of the message queue.
data LogQueue = LogQueue !(IORef [Maybe (WarnReason, Severity, SrcSpan, PprStyle, MsgDoc)])
                         !(MVar ())

-- | The graph of modules to compile and their corresponding result 'MVar' and
-- 'LogQueue'.
type CompilationGraph = [(ModSummary, MVar SuccessFlag, LogQueue)]

-- | Build a 'CompilationGraph' out of a list of strongly-connected modules,
-- also returning the first, if any, encountered module cycle.
buildCompGraph :: [SCC ModSummary] -> IO (CompilationGraph, Maybe [ModSummary])
buildCompGraph [] = return ([], Nothing)
buildCompGraph (scc:sccs) = case scc of
    AcyclicSCC ms -> do
        mvar <- newEmptyMVar
        log_queue <- do
            ref <- newIORef []
            sem <- newEmptyMVar
            return (LogQueue ref sem)
        (rest,cycle) <- buildCompGraph sccs
        return ((ms,mvar,log_queue):rest, cycle)
    CyclicSCC mss -> return ([], Just mss)

-- A Module and whether it is a boot module.
type BuildModule = (Module, IsBoot)

-- | 'Bool' indicating if a module is a boot module or not.  We need to treat
-- boot modules specially when building compilation graphs, since they break
-- cycles.  Regular source files and signature files are treated equivalently.
data IsBoot = NotBoot | IsBoot
    deriving (Ord, Eq, Show, Read)

-- | Tests if an 'HscSource' is a boot file, primarily for constructing
-- elements of 'BuildModule'.
hscSourceToIsBoot :: HscSource -> IsBoot
hscSourceToIsBoot HsBootFile = IsBoot
hscSourceToIsBoot _ = NotBoot

mkBuildModule :: ModSummary -> BuildModule
mkBuildModule ms = (ms_mod ms, if isBootSummary ms then IsBoot else NotBoot)

-- | The entry point to the parallel upsweep.
--
-- See also the simpler, sequential 'upsweep'.
parUpsweep
    :: GhcMonad m
    => Int
    -- ^ The number of workers we wish to run in parallel
    -> Maybe Messager
    -> HomePackageTable
    -> StableModules
    -> (HscEnv -> IO ())
    -> [SCC ModSummary]
    -> m (SuccessFlag,
          [ModSummary])
parUpsweep n_jobs mHscMessage old_hpt stable_mods cleanup sccs = do
    hsc_env <- getSession
    let dflags = hsc_dflags hsc_env

    when (not (null (unitIdsToCheck dflags))) $
      throwGhcException (ProgramError "Backpack typechecking not supported with -j")

    -- The bits of shared state we'll be using:

    -- The global HscEnv is updated with the module's HMI when a module
    -- successfully compiles.
    hsc_env_var <- liftIO $ newMVar hsc_env

    -- The old HPT is used for recompilation checking in upsweep_mod. When a
    -- module successfully gets compiled, its HMI is pruned from the old HPT.
    old_hpt_var <- liftIO $ newIORef old_hpt

    -- What we use to limit parallelism with.
    par_sem <- liftIO $ newQSem n_jobs


    let updNumCapabilities = liftIO $ do
            n_capabilities <- getNumCapabilities
            n_cpus <- getNumProcessors
            -- Setting number of capabilities more than
            -- CPU count usually leads to high userspace
            -- lock contention. #9221
            let n_caps = min n_jobs n_cpus
            unless (n_capabilities /= 1) $ setNumCapabilities n_caps
            return n_capabilities
    -- Reset the number of capabilities once the upsweep ends.
    let resetNumCapabilities orig_n = liftIO $ setNumCapabilities orig_n

    gbracket updNumCapabilities resetNumCapabilities $ \_ -> do

    -- Sync the global session with the latest HscEnv once the upsweep ends.
    let finallySyncSession io = io `gfinally` do
            hsc_env <- liftIO $ readMVar hsc_env_var
            setSession hsc_env

    finallySyncSession $ do

    -- Build the compilation graph out of the list of SCCs. Module cycles are
    -- handled at the very end, after some useful work gets done. Note that
    -- this list is topologically sorted (by virtue of 'sccs' being sorted so).
    (comp_graph,cycle) <- liftIO $ buildCompGraph sccs
    let comp_graph_w_idx = zip comp_graph [1..]

    -- The list of all loops in the compilation graph.
    -- NB: For convenience, the last module of each loop (aka the module that
    -- finishes the loop) is prepended to the beginning of the loop.
    let graph = map fstOf3 (reverse comp_graph)
        boot_modules = mkModuleSet [ms_mod ms | ms <- graph, isBootSummary ms]
        comp_graph_loops = go graph boot_modules
          where
            remove ms bm
              | isBootSummary ms = delModuleSet bm (ms_mod ms)
              | otherwise = bm
            go [] _ = []
            go mg@(ms:mss) boot_modules
              | Just loop <- getModLoop ms mg (`elemModuleSet` boot_modules)
              = map mkBuildModule (ms:loop) : go mss (remove ms boot_modules)
              | otherwise
              = go mss (remove ms boot_modules)

    -- Build a Map out of the compilation graph with which we can efficiently
    -- look up the result MVar associated with a particular home module.
    let home_mod_map :: Map BuildModule (MVar SuccessFlag, Int)
        home_mod_map =
            Map.fromList [ (mkBuildModule ms, (mvar, idx))
                         | ((ms,mvar,_),idx) <- comp_graph_w_idx ]


    liftIO $ label_self "main --make thread"
    -- For each module in the module graph, spawn a worker thread that will
    -- compile this module.
    let { spawnWorkers = forM comp_graph_w_idx $ \((mod,!mvar,!log_queue),!mod_idx) ->
            forkIOWithUnmask $ \unmask -> do
                liftIO $ label_self $ unwords
                    [ "worker --make thread"
                    , "for module"
                    , show (moduleNameString (ms_mod_name mod))
                    , "number"
                    , show mod_idx
                    ]
                -- Replace the default log_action with one that writes each
                -- message to the module's log_queue. The main thread will
                -- deal with synchronously printing these messages.
                --
                -- Use a local filesToClean var so that we can clean up
                -- intermediate files in a timely fashion (as soon as
                -- compilation for that module is finished) without having to
                -- worry about accidentally deleting a simultaneous compile's
                -- important files.
                lcl_files_to_clean <- newIORef emptyFilesToClean
                let lcl_dflags = dflags { log_action = parLogAction log_queue
                                        , filesToClean = lcl_files_to_clean }

                -- Unmask asynchronous exceptions and perform the thread-local
                -- work to compile the module (see parUpsweep_one).
                m_res <- try $ unmask $ prettyPrintGhcErrors lcl_dflags $
                        parUpsweep_one mod home_mod_map comp_graph_loops
                                       lcl_dflags mHscMessage cleanup
                                       par_sem hsc_env_var old_hpt_var
                                       stable_mods mod_idx (length sccs)

                res <- case m_res of
                    Right flag -> return flag
                    Left exc -> do
                        -- Don't print ThreadKilled exceptions: they are used
                        -- to kill the worker thread in the event of a user
                        -- interrupt, and the user doesn't have to be informed
                        -- about that.
                        when (fromException exc /= Just ThreadKilled)
                             (errorMsg lcl_dflags (text (show exc)))
                        return Failed

                -- Populate the result MVar.
                putMVar mvar res

                -- Write the end marker to the message queue, telling the main
                -- thread that it can stop waiting for messages from this
                -- particular compile.
                writeLogQueue log_queue Nothing

                -- Add the remaining files that weren't cleaned up to the
                -- global filesToClean ref, for cleanup later.
                FilesToClean
                  { ftcCurrentModule = cm_files
                  , ftcGhcSession = gs_files
                  } <- readIORef (filesToClean lcl_dflags)
                addFilesToClean dflags TFL_CurrentModule $ Set.toList cm_files
                addFilesToClean dflags TFL_GhcSession $ Set.toList gs_files

        -- Kill all the workers, masking interrupts (since killThread is
        -- interruptible). XXX: This is not ideal.
        ; killWorkers = uninterruptibleMask_ . mapM_ killThread }


    -- Spawn the workers, making sure to kill them later. Collect the results
    -- of each compile.
    results <- liftIO $ bracket spawnWorkers killWorkers $ \_ ->
        -- Loop over each module in the compilation graph in order, printing
        -- each message from its log_queue.
        forM comp_graph $ \(mod,mvar,log_queue) -> do
            printLogs dflags log_queue
            result <- readMVar mvar
            if succeeded result then return (Just mod) else return Nothing


    -- Collect and return the ModSummaries of all the successful compiles.
    -- NB: Reverse this list to maintain output parity with the sequential upsweep.
    let ok_results = reverse (catMaybes results)

    -- Handle any cycle in the original compilation graph and return the result
    -- of the upsweep.
    case cycle of
        Just mss -> do
            liftIO $ fatalErrorMsg dflags (cyclicModuleErr mss)
            return (Failed,ok_results)
        Nothing  -> do
            let success_flag = successIf (all isJust results)
            return (success_flag,ok_results)

  where
    writeLogQueue :: LogQueue -> Maybe (WarnReason,Severity,SrcSpan,PprStyle,MsgDoc) -> IO ()
    writeLogQueue (LogQueue ref sem) msg = do
        atomicModifyIORef' ref $ \msgs -> (msg:msgs,())
        _ <- tryPutMVar sem ()
        return ()

    -- The log_action callback that is used to synchronize messages from a
    -- worker thread.
    parLogAction :: LogQueue -> LogAction
    parLogAction log_queue _dflags !reason !severity !srcSpan !style !msg = do
        writeLogQueue log_queue (Just (reason,severity,srcSpan,style,msg))

    -- Print each message from the log_queue using the log_action from the
    -- session's DynFlags.
    printLogs :: DynFlags -> LogQueue -> IO ()
    printLogs !dflags (LogQueue ref sem) = read_msgs
      where read_msgs = do
                takeMVar sem
                msgs <- atomicModifyIORef' ref $ \xs -> ([], reverse xs)
                print_loop msgs

            print_loop [] = read_msgs
            print_loop (x:xs) = case x of
                Just (reason,severity,srcSpan,style,msg) -> do
                    putLogMsg dflags reason severity srcSpan style msg
                    print_loop xs
                -- Exit the loop once we encounter the end marker.
                Nothing -> return ()

-- The interruptible subset of the worker threads' work.
parUpsweep_one
    :: ModSummary
    -- ^ The module we wish to compile
    -> Map BuildModule (MVar SuccessFlag, Int)
    -- ^ The map of home modules and their result MVar
    -> [[BuildModule]]
    -- ^ The list of all module loops within the compilation graph.
    -> DynFlags
    -- ^ The thread-local DynFlags
    -> Maybe Messager
    -- ^ The messager
    -> (HscEnv -> IO ())
    -- ^ The callback for cleaning up intermediate files
    -> QSem
    -- ^ The semaphore for limiting the number of simultaneous compiles
    -> MVar HscEnv
    -- ^ The MVar that synchronizes updates to the global HscEnv
    -> IORef HomePackageTable
    -- ^ The old HPT
    -> StableModules
    -- ^ Sets of stable objects and BCOs
    -> Int
    -- ^ The index of this module
    -> Int
    -- ^ The total number of modules
    -> IO SuccessFlag
    -- ^ The result of this compile
parUpsweep_one mod home_mod_map comp_graph_loops lcl_dflags mHscMessage cleanup par_sem
               hsc_env_var old_hpt_var stable_mods mod_index num_mods = do

    let this_build_mod = mkBuildModule mod

    let home_imps     = map unLoc $ ms_home_imps mod
    let home_src_imps = map unLoc $ ms_home_srcimps mod

    -- All the textual imports of this module.
    let textual_deps = Set.fromList $ mapFst (mkModule (thisPackage lcl_dflags)) $
                            zip home_imps     (repeat NotBoot) ++
                            zip home_src_imps (repeat IsBoot)

    -- Dealing with module loops
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~
    --
    -- Not only do we have to deal with explicit textual dependencies, we also
    -- have to deal with implicit dependencies introduced by import cycles that
    -- are broken by an hs-boot file. We have to ensure that:
    --
    -- 1. A module that breaks a loop must depend on all the modules in the
    --    loop (transitively or otherwise). This is normally always fulfilled
    --    by the module's textual dependencies except in degenerate loops,
    --    e.g.:
    --
    --    A.hs imports B.hs-boot
    --    B.hs doesn't import A.hs
    --    C.hs imports A.hs, B.hs
    --
    --    In this scenario, getModLoop will detect the module loop [A,B] but
    --    the loop finisher B doesn't depend on A. So we have to explicitly add
    --    A in as a dependency of B when we are compiling B.
    --
    -- 2. A module that depends on a module in an external loop can't proceed
    --    until the entire loop is re-typechecked.
    --
    -- These two invariants have to be maintained to correctly build a
    -- compilation graph with one or more loops.


    -- The loop that this module will finish. After this module successfully
    -- compiles, this loop is going to get re-typechecked.
    let finish_loop = listToMaybe
            [ tail loop | loop <- comp_graph_loops
                        , head loop == this_build_mod ]

    -- If this module finishes a loop then it must depend on all the other
    -- modules in that loop because the entire module loop is going to be
    -- re-typechecked once this module gets compiled. These extra dependencies
    -- are this module's "internal" loop dependencies, because this module is
    -- inside the loop in question.
    let int_loop_deps = Set.fromList $
            case finish_loop of
                Nothing   -> []
                Just loop -> filter (/= this_build_mod) loop

    -- If this module depends on a module within a loop then it must wait for
    -- that loop to get re-typechecked, i.e. it must wait on the module that
    -- finishes that loop. These extra dependencies are this module's
    -- "external" loop dependencies, because this module is outside of the
    -- loop(s) in question.
    let ext_loop_deps = Set.fromList
            [ head loop | loop <- comp_graph_loops
                        , any (`Set.member` textual_deps) loop
                        , this_build_mod `notElem` loop ]


    let all_deps = foldl1 Set.union [textual_deps, int_loop_deps, ext_loop_deps]

    -- All of the module's home-module dependencies.
    let home_deps_with_idx =
            [ home_dep | dep <- Set.toList all_deps
                       , Just home_dep <- [Map.lookup dep home_mod_map] ]

    -- Sort the list of dependencies in reverse-topological order. This way, by
    -- the time we get woken up by the result of an earlier dependency,
    -- subsequent dependencies are more likely to have finished. This step
    -- effectively reduces the number of MVars that each thread blocks on.
    let home_deps = map fst $ sortBy (flip (comparing snd)) home_deps_with_idx

    -- Wait for the all the module's dependencies to finish building.
    deps_ok <- allM (fmap succeeded . readMVar) home_deps

    -- We can't build this module if any of its dependencies failed to build.
    if not deps_ok
      then return Failed
      else do
        -- Any hsc_env at this point is OK to use since we only really require
        -- that the HPT contains the HMIs of our dependencies.
        hsc_env <- readMVar hsc_env_var
        old_hpt <- readIORef old_hpt_var

        let logger err = printBagOfErrors lcl_dflags (srcErrorMessages err)

        -- Limit the number of parallel compiles.
        let withSem sem = bracket_ (waitQSem sem) (signalQSem sem)
        mb_mod_info <- withSem par_sem $
            handleSourceError (\err -> do logger err; return Nothing) $ do
                -- Have the ModSummary and HscEnv point to our local log_action
                -- and filesToClean var.
                let lcl_mod = localize_mod mod
                let lcl_hsc_env = localize_hsc_env hsc_env

                -- Re-typecheck the loop
                -- This is necessary to make sure the knot is tied when
                -- we close a recursive module loop, see bug #12035.
                type_env_var <- liftIO $ newIORef emptyNameEnv
                let lcl_hsc_env' = lcl_hsc_env { hsc_type_env_var =
                                    Just (ms_mod lcl_mod, type_env_var) }
                lcl_hsc_env'' <- case finish_loop of
                    Nothing   -> return lcl_hsc_env'
                    -- In the non-parallel case, the retypecheck prior to
                    -- typechecking the loop closer includes all modules
                    -- EXCEPT the loop closer.  However, our precomputed
                    -- SCCs include the loop closer, so we have to filter
                    -- it out.
                    Just loop -> typecheckLoop lcl_dflags lcl_hsc_env' $
                                 filter (/= moduleName (fst this_build_mod)) $
                                 map (moduleName . fst) loop

                -- Compile the module.
                mod_info <- upsweep_mod lcl_hsc_env'' mHscMessage old_hpt stable_mods
                                        lcl_mod mod_index num_mods
                return (Just mod_info)

        case mb_mod_info of
            Nothing -> return Failed
            Just mod_info -> do
                let this_mod = ms_mod_name mod

                -- Prune the old HPT unless this is an hs-boot module.
                unless (isBootSummary mod) $
                    atomicModifyIORef' old_hpt_var $ \old_hpt ->
                        (delFromHpt old_hpt this_mod, ())

                -- Update and fetch the global HscEnv.
                lcl_hsc_env' <- modifyMVar hsc_env_var $ \hsc_env -> do
                    let hsc_env' = hsc_env
                                     { hsc_HPT = addToHpt (hsc_HPT hsc_env)
                                                           this_mod mod_info }
                    -- We've finished typechecking the module, now we must
                    -- retypecheck the loop AGAIN to ensure unfoldings are
                    -- updated.  This time, however, we include the loop
                    -- closer!
                    hsc_env'' <- case finish_loop of
                        Nothing   -> return hsc_env'
                        Just loop -> typecheckLoop lcl_dflags hsc_env' $
                                     map (moduleName . fst) loop
                    return (hsc_env'', localize_hsc_env hsc_env'')

                -- Clean up any intermediate files.
                cleanup lcl_hsc_env'
                return Succeeded

  where
    localize_mod mod
        = mod { ms_hspp_opts = (ms_hspp_opts mod)
                 { log_action = log_action lcl_dflags
                 , filesToClean = filesToClean lcl_dflags } }

    localize_hsc_env hsc_env
        = hsc_env { hsc_dflags = (hsc_dflags hsc_env)
                     { log_action = log_action lcl_dflags
                     , filesToClean = filesToClean lcl_dflags } }

-- -----------------------------------------------------------------------------
--
-- | The upsweep
--
-- This is where we compile each module in the module graph, in a pass
-- from the bottom to the top of the graph.
--
-- There better had not be any cyclic groups here -- we check for them.
upsweep
    :: GhcMonad m
    => Maybe Messager
    -> HomePackageTable            -- ^ HPT from last time round (pruned)
    -> StableModules               -- ^ stable modules (see checkStability)
    -> (HscEnv -> IO ())           -- ^ How to clean up unwanted tmp files
    -> [SCC ModSummary]            -- ^ Mods to do (the worklist)
    -> m (SuccessFlag,
          [ModSummary])
       -- ^ Returns:
       --
       --  1. A flag whether the complete upsweep was successful.
       --  2. The 'HscEnv' in the monad has an updated HPT
       --  3. A list of modules which succeeded loading.

upsweep mHscMessage old_hpt stable_mods cleanup sccs = do
   dflags <- getSessionDynFlags
   (res, done) <- upsweep' old_hpt emptyMG sccs 1 (length sccs)
                           (unitIdsToCheck dflags) done_holes
   return (res, reverse $ mgModSummaries done)
 where
  done_holes = emptyUniqSet

  keep_going this_mods old_hpt done mods mod_index nmods uids_to_check done_holes = do
    let sum_deps ms (AcyclicSCC mod) =
          if any (flip elem . map (unLoc . snd) $ ms_imps mod) ms
            then ms_mod_name mod:ms
            else ms
        sum_deps ms _ = ms
        dep_closure = foldl' sum_deps this_mods mods
        dropped_ms = drop (length this_mods) (reverse dep_closure)
        prunable (AcyclicSCC mod) = elem (ms_mod_name mod) dep_closure
        prunable _ = False
        mods' = filter (not . prunable) mods
        nmods' = nmods - length dropped_ms

    when (not $ null dropped_ms) $ do
        dflags <- getSessionDynFlags
        liftIO $ fatalErrorMsg dflags (keepGoingPruneErr dropped_ms)
    (_, done') <- upsweep' old_hpt done mods' (mod_index+1) nmods' uids_to_check done_holes
    return (Failed, done')

  upsweep'
    :: GhcMonad m
    => HomePackageTable
    -> ModuleGraph
    -> [SCC ModSummary]
    -> Int
    -> Int
    -> [UnitId]
    -> UniqSet ModuleName
    -> m (SuccessFlag, ModuleGraph)
  upsweep' _old_hpt done
     [] _ _ uids_to_check _
   = do hsc_env <- getSession
        liftIO . runHsc hsc_env $ mapM_ (ioMsgMaybe . tcRnCheckUnitId hsc_env) uids_to_check
        return (Succeeded, done)

  upsweep' _old_hpt done
     (CyclicSCC ms:mods) mod_index nmods uids_to_check done_holes
   = do dflags <- getSessionDynFlags
        liftIO $ fatalErrorMsg dflags (cyclicModuleErr ms)
        if gopt Opt_KeepGoing dflags
          then keep_going (map ms_mod_name ms) old_hpt done mods mod_index nmods
                          uids_to_check done_holes
          else return (Failed, done)

  upsweep' old_hpt done
     (AcyclicSCC mod:mods) mod_index nmods uids_to_check done_holes
   = do -- putStrLn ("UPSWEEP_MOD: hpt = " ++
        --           show (map (moduleUserString.moduleName.mi_module.hm_iface)
        --                     (moduleEnvElts (hsc_HPT hsc_env)))
        let logger _mod = defaultWarnErrLogger

        hsc_env <- getSession

        -- TODO: Cache this, so that we don't repeatedly re-check
        -- our imports when you run --make.
        let (ready_uids, uids_to_check')
                = partition (\uid -> isEmptyUniqDSet
                    (unitIdFreeHoles uid `uniqDSetMinusUniqSet` done_holes))
                     uids_to_check
            done_holes'
                | ms_hsc_src mod == HsigFile
                = addOneToUniqSet done_holes (ms_mod_name mod)
                | otherwise = done_holes
        liftIO . runHsc hsc_env $ mapM_ (ioMsgMaybe . tcRnCheckUnitId hsc_env) ready_uids

        -- Remove unwanted tmp files between compilations
        liftIO (cleanup hsc_env)

        -- Get ready to tie the knot
        type_env_var <- liftIO $ newIORef emptyNameEnv
        let hsc_env1 = hsc_env { hsc_type_env_var =
                                    Just (ms_mod mod, type_env_var) }
        setSession hsc_env1

        -- Lazily reload the HPT modules participating in the loop.
        -- See Note [Tying the knot]--if we don't throw out the old HPT
        -- and reinitalize the knot-tying process, anything that was forced
        -- while we were previously typechecking won't get updated, this
        -- was bug #12035.
        hsc_env2 <- liftIO $ reTypecheckLoop hsc_env1 mod done
        setSession hsc_env2

        mb_mod_info
            <- handleSourceError
                   (\err -> do logger mod (Just err); return Nothing) $ do
                 mod_info <- liftIO $ upsweep_mod hsc_env2 mHscMessage old_hpt stable_mods
                                                  mod mod_index nmods
                 logger mod Nothing -- log warnings
                 return (Just mod_info)

        case mb_mod_info of
          Nothing -> do
                dflags <- getSessionDynFlags
                if gopt Opt_KeepGoing dflags
                  then keep_going [ms_mod_name mod] old_hpt done mods mod_index nmods
                                  uids_to_check done_holes
                  else return (Failed, done)
          Just mod_info -> do
                let this_mod = ms_mod_name mod

                        -- Add new info to hsc_env
                    hpt1     = addToHpt (hsc_HPT hsc_env2) this_mod mod_info
                    hsc_env3 = hsc_env2 { hsc_HPT = hpt1, hsc_type_env_var = Nothing }

                        -- Space-saving: delete the old HPT entry
                        -- for mod BUT if mod is a hs-boot
                        -- node, don't delete it.  For the
                        -- interface, the HPT entry is probably for the
                        -- main Haskell source file.  Deleting it
                        -- would force the real module to be recompiled
                        -- every time.
                    old_hpt1 | isBootSummary mod = old_hpt
                             | otherwise = delFromHpt old_hpt this_mod

                    done' = extendMG done mod

                        -- fixup our HomePackageTable after we've finished compiling
                        -- a mutually-recursive loop.  We have to do this again
                        -- to make sure we have the final unfoldings, which may
                        -- not have been computed accurately in the previous
                        -- retypecheck.
                hsc_env4 <- liftIO $ reTypecheckLoop hsc_env3 mod done'
                setSession hsc_env4

                        -- Add any necessary entries to the static pointer
                        -- table. See Note [Grand plan for static forms] in
                        -- StaticPtrTable.
                when (hscTarget (hsc_dflags hsc_env4) == HscInterpreted) $
                    liftIO $ hscAddSptEntries hsc_env4
                                 [ spt
                                 | Just linkable <- pure $ hm_linkable mod_info
                                 , unlinked <- linkableUnlinked linkable
                                 , BCOs _ spts <- pure unlinked
                                 , spt <- spts
                                 ]

                upsweep' old_hpt1 done' mods (mod_index+1) nmods uids_to_check' done_holes'

unitIdsToCheck :: DynFlags -> [UnitId]
unitIdsToCheck dflags =
  nubSort $ concatMap goUnitId (explicitPackages (pkgState dflags))
 where
  goUnitId uid =
    case splitUnitIdInsts uid of
      (_, Just indef) ->
        let insts = indefUnitIdInsts indef
        in uid : concatMap (goUnitId . moduleUnitId . snd) insts
      _ -> []

maybeGetIfaceDate :: DynFlags -> ModLocation -> IO (Maybe UTCTime)
maybeGetIfaceDate dflags location
 | writeInterfaceOnlyMode dflags
    -- Minor optimization: it should be harmless to check the hi file location
    -- always, but it's better to avoid hitting the filesystem if possible.
    = modificationTimeIfExists (ml_hi_file location)
 | otherwise
    = return Nothing

-- | Compile a single module.  Always produce a Linkable for it if
-- successful.  If no compilation happened, return the old Linkable.
upsweep_mod :: HscEnv
            -> Maybe Messager
            -> HomePackageTable
            -> StableModules
            -> ModSummary
            -> Int  -- index of module
            -> Int  -- total number of modules
            -> IO HomeModInfo
upsweep_mod hsc_env mHscMessage old_hpt (stable_obj, stable_bco) summary mod_index nmods
   =    let
            this_mod_name = ms_mod_name summary
            this_mod    = ms_mod summary
            mb_obj_date = ms_obj_date summary
            mb_if_date  = ms_iface_date summary
            obj_fn      = ml_obj_file (ms_location summary)
            hs_date     = ms_hs_date summary

            is_stable_obj = this_mod_name `elementOfUniqSet` stable_obj
            is_stable_bco = this_mod_name `elementOfUniqSet` stable_bco

            old_hmi = lookupHpt old_hpt this_mod_name

            -- We're using the dflags for this module now, obtained by
            -- applying any options in its LANGUAGE & OPTIONS_GHC pragmas.
            dflags = ms_hspp_opts summary
            prevailing_target = hscTarget (hsc_dflags hsc_env)
            local_target      = hscTarget dflags

            -- If OPTIONS_GHC contains -fasm or -fllvm, be careful that
            -- we don't do anything dodgy: these should only work to change
            -- from -fllvm to -fasm and vice-versa, or away from -fno-code,
            -- otherwise we could end up trying to link object code to byte
            -- code.
            target = if prevailing_target /= local_target
                        && (not (isObjectTarget prevailing_target)
                            || not (isObjectTarget local_target))
                        && not (prevailing_target == HscNothing)
                        && not (prevailing_target == HscInterpreted)
                        then prevailing_target
                        else local_target

            -- store the corrected hscTarget into the summary
            summary' = summary{ ms_hspp_opts = dflags { hscTarget = target } }

            -- The old interface is ok if
            --  a) we're compiling a source file, and the old HPT
            --     entry is for a source file
            --  b) we're compiling a hs-boot file
            -- Case (b) allows an hs-boot file to get the interface of its
            -- real source file on the second iteration of the compilation
            -- manager, but that does no harm.  Otherwise the hs-boot file
            -- will always be recompiled

            mb_old_iface
                = case old_hmi of
                     Nothing                              -> Nothing
                     Just hm_info | isBootSummary summary -> Just iface
                                  | not (mi_boot iface)   -> Just iface
                                  | otherwise             -> Nothing
                                   where
                                     iface = hm_iface hm_info

            compile_it :: Maybe Linkable -> SourceModified -> IO HomeModInfo
            compile_it  mb_linkable src_modified =
                  compileOne' Nothing mHscMessage hsc_env summary' mod_index nmods
                             mb_old_iface mb_linkable src_modified

            compile_it_discard_iface :: Maybe Linkable -> SourceModified
                                     -> IO HomeModInfo
            compile_it_discard_iface mb_linkable  src_modified =
                  compileOne' Nothing mHscMessage hsc_env summary' mod_index nmods
                             Nothing mb_linkable src_modified

            -- With the HscNothing target we create empty linkables to avoid
            -- recompilation.  We have to detect these to recompile anyway if
            -- the target changed since the last compile.
            is_fake_linkable
               | Just hmi <- old_hmi, Just l <- hm_linkable hmi =
                  null (linkableUnlinked l)
               | otherwise =
                   -- we have no linkable, so it cannot be fake
                   False

            implies False _ = True
            implies True x  = x

        in
        case () of
         _
                -- Regardless of whether we're generating object code or
                -- byte code, we can always use an existing object file
                -- if it is *stable* (see checkStability).
          | is_stable_obj, Just hmi <- old_hmi -> do
                liftIO $ debugTraceMsg (hsc_dflags hsc_env) 5
                           (text "skipping stable obj mod:" <+> ppr this_mod_name)
                return hmi
                -- object is stable, and we have an entry in the
                -- old HPT: nothing to do

          | is_stable_obj, isNothing old_hmi -> do
                liftIO $ debugTraceMsg (hsc_dflags hsc_env) 5
                           (text "compiling stable on-disk mod:" <+> ppr this_mod_name)
                linkable <- liftIO $ findObjectLinkable this_mod obj_fn
                              (expectJust "upsweep1" mb_obj_date)
                compile_it (Just linkable) SourceUnmodifiedAndStable
                -- object is stable, but we need to load the interface
                -- off disk to make a HMI.

          | not (isObjectTarget target), is_stable_bco,
            (target /= HscNothing) `implies` not is_fake_linkable ->
                ASSERT(isJust old_hmi) -- must be in the old_hpt
                let Just hmi = old_hmi in do
                liftIO $ debugTraceMsg (hsc_dflags hsc_env) 5
                           (text "skipping stable BCO mod:" <+> ppr this_mod_name)
                return hmi
                -- BCO is stable: nothing to do

          | not (isObjectTarget target),
            Just hmi <- old_hmi,
            Just l <- hm_linkable hmi,
            not (isObjectLinkable l),
            (target /= HscNothing) `implies` not is_fake_linkable,
            linkableTime l >= ms_hs_date summary -> do
                liftIO $ debugTraceMsg (hsc_dflags hsc_env) 5
                           (text "compiling non-stable BCO mod:" <+> ppr this_mod_name)
                compile_it (Just l) SourceUnmodified
                -- we have an old BCO that is up to date with respect
                -- to the source: do a recompilation check as normal.

          -- When generating object code, if there's an up-to-date
          -- object file on the disk, then we can use it.
          -- However, if the object file is new (compared to any
          -- linkable we had from a previous compilation), then we
          -- must discard any in-memory interface, because this
          -- means the user has compiled the source file
          -- separately and generated a new interface, that we must
          -- read from the disk.
          --
          | isObjectTarget target,
            Just obj_date <- mb_obj_date,
            obj_date >= hs_date -> do
                case old_hmi of
                  Just hmi
                    | Just l <- hm_linkable hmi,
                      isObjectLinkable l && linkableTime l == obj_date -> do
                          liftIO $ debugTraceMsg (hsc_dflags hsc_env) 5
                                     (text "compiling mod with new on-disk obj:" <+> ppr this_mod_name)
                          compile_it (Just l) SourceUnmodified
                  _otherwise -> do
                          liftIO $ debugTraceMsg (hsc_dflags hsc_env) 5
                                     (text "compiling mod with new on-disk obj2:" <+> ppr this_mod_name)
                          linkable <- liftIO $ findObjectLinkable this_mod obj_fn obj_date
                          compile_it_discard_iface (Just linkable) SourceUnmodified

          -- See Note [Recompilation checking in -fno-code mode]
          | writeInterfaceOnlyMode dflags,
            Just if_date <- mb_if_date,
            if_date >= hs_date -> do
                liftIO $ debugTraceMsg (hsc_dflags hsc_env) 5
                           (text "skipping tc'd mod:" <+> ppr this_mod_name)
                compile_it Nothing SourceUnmodified

         _otherwise -> do
                liftIO $ debugTraceMsg (hsc_dflags hsc_env) 5
                           (text "compiling mod:" <+> ppr this_mod_name)
                compile_it Nothing SourceModified


{- Note [-fno-code mode]
~~~~~~~~~~~~~~~~~~~~~~~~
GHC offers the flag -fno-code for the purpose of parsing and typechecking a
program without generating object files. This is intended to be used by tooling
and IDEs to provide quick feedback on any parser or type errors as cheaply as
possible.

When GHC is invoked with -fno-code no object files or linked output will be
generated. As many errors and warnings as possible will be generated, as if
-fno-code had not been passed. The session DynFlags will have
hscTarget == HscNothing.

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

-- Note [Recompilation checking in -fno-code mode]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- If we are compiling with -fno-code -fwrite-interface, there won't
-- be any object code that we can compare against, nor should there
-- be: we're *just* generating interface files.  In this case, we
-- want to check if the interface file is new, in lieu of the object
-- file.  See also #9243.

-- Filter modules in the HPT
retainInTopLevelEnvs :: [ModuleName] -> HomePackageTable -> HomePackageTable
retainInTopLevelEnvs keep_these hpt
   = listToHpt   [ (mod, expectJust "retain" mb_mod_info)
                 | mod <- keep_these
                 , let mb_mod_info = lookupHpt hpt mod
                 , isJust mb_mod_info ]

-- ---------------------------------------------------------------------------
-- Typecheck module loops
{-
See bug #930.  This code fixes a long-standing bug in --make.  The
problem is that when compiling the modules *inside* a loop, a data
type that is only defined at the top of the loop looks opaque; but
after the loop is done, the structure of the data type becomes
apparent.

The difficulty is then that two different bits of code have
different notions of what the data type looks like.

The idea is that after we compile a module which also has an .hs-boot
file, we re-generate the ModDetails for each of the modules that
depends on the .hs-boot file, so that everyone points to the proper
TyCons, Ids etc. defined by the real module, not the boot module.
Fortunately re-generating a ModDetails from a ModIface is easy: the
function GHC.IfaceToCore.typecheckIface does exactly that.

Picking the modules to re-typecheck is slightly tricky.  Starting from
the module graph consisting of the modules that have already been
compiled, we reverse the edges (so they point from the imported module
to the importing module), and depth-first-search from the .hs-boot
node.  This gives us all the modules that depend transitively on the
.hs-boot module, and those are exactly the modules that we need to
re-typecheck.

Following this fix, GHC can compile itself with --make -O2.
-}

reTypecheckLoop :: HscEnv -> ModSummary -> ModuleGraph -> IO HscEnv
reTypecheckLoop hsc_env ms graph
  | Just loop <- getModLoop ms mss appearsAsBoot
  -- SOME hs-boot files should still
  -- get used, just not the loop-closer.
  , let non_boot = filter (\l -> not (isBootSummary l &&
                                 ms_mod l == ms_mod ms)) loop
  = typecheckLoop (hsc_dflags hsc_env) hsc_env (map ms_mod_name non_boot)
  | otherwise
  = return hsc_env
  where
  mss = mgModSummaries graph
  appearsAsBoot = (`elemModuleSet` mgBootModules graph)

-- | Given a non-boot ModSummary @ms@ of a module, for which there exists a
-- corresponding boot file in @graph@, return the set of modules which
-- transitively depend on this boot file.  This function is slightly misnamed,
-- but its name "getModLoop" alludes to the fact that, when getModLoop is called
-- with a graph that does not contain @ms@ (non-parallel case) or is an
-- SCC with hs-boot nodes dropped (parallel-case), the modules which
-- depend on the hs-boot file are typically (but not always) the
-- modules participating in the recursive module loop.  The returned
-- list includes the hs-boot file.
--
-- Example:
--      let g represent the module graph:
--          C.hs
--          A.hs-boot imports C.hs
--          B.hs imports A.hs-boot
--          A.hs imports B.hs
--      genModLoop A.hs g == Just [A.hs-boot, B.hs, A.hs]
--
--      It would also be permissible to omit A.hs from the graph,
--      in which case the result is [A.hs-boot, B.hs]
--
-- Example:
--      A counter-example to the claim that modules returned
--      by this function participate in the loop occurs here:
--
--      let g represent the module graph:
--          C.hs
--          A.hs-boot imports C.hs
--          B.hs imports A.hs-boot
--          A.hs imports B.hs
--          D.hs imports A.hs-boot
--      genModLoop A.hs g == Just [A.hs-boot, B.hs, A.hs, D.hs]
--
--      Arguably, D.hs should import A.hs, not A.hs-boot, but
--      a dependency on the boot file is not illegal.
--
getModLoop
  :: ModSummary
  -> [ModSummary]
  -> (Module -> Bool) -- check if a module appears as a boot module in 'graph'
  -> Maybe [ModSummary]
getModLoop ms graph appearsAsBoot
  | not (isBootSummary ms)
  , appearsAsBoot this_mod
  , let mss = reachableBackwards (ms_mod_name ms) graph
  = Just mss
  | otherwise
  = Nothing
 where
  this_mod = ms_mod ms

-- NB: sometimes mods has duplicates; this is harmless because
-- any duplicates get clobbered in addListToHpt and never get forced.
typecheckLoop :: DynFlags -> HscEnv -> [ModuleName] -> IO HscEnv
typecheckLoop dflags hsc_env mods = do
  debugTraceMsg dflags 2 $
     text "Re-typechecking loop: " <> ppr mods
  new_hpt <-
    fixIO $ \new_hpt -> do
      let new_hsc_env = hsc_env{ hsc_HPT = new_hpt }
      mds <- initIfaceCheck (text "typecheckLoop") new_hsc_env $
                mapM (typecheckIface . hm_iface) hmis
      let new_hpt = addListToHpt old_hpt
                        (zip mods [ hmi{ hm_details = details }
                                  | (hmi,details) <- zip hmis mds ])
      return new_hpt
  return hsc_env{ hsc_HPT = new_hpt }
  where
    old_hpt = hsc_HPT hsc_env
    hmis    = map (expectJust "typecheckLoop" . lookupHpt old_hpt) mods

reachableBackwards :: ModuleName -> [ModSummary] -> [ModSummary]
reachableBackwards mod summaries
  = [ node_payload node | node <- reachableG (transposeG graph) root ]
  where -- the rest just sets up the graph:
        (graph, lookup_node) = moduleGraphNodes False summaries
        root  = expectJust "reachableBackwards" (lookup_node HsBootFile mod)

-- ---------------------------------------------------------------------------
--
-- | Topological sort of the module graph
topSortModuleGraph
          :: Bool
          -- ^ Drop hi-boot nodes? (see below)
          -> ModuleGraph
          -> Maybe ModuleName
             -- ^ Root module name.  If @Nothing@, use the full graph.
          -> [SCC ModSummary]
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

topSortModuleGraph drop_hs_boot_nodes module_graph mb_root_mod
  = map (fmap summaryNodeSummary) $ stronglyConnCompG initial_graph
  where
    summaries = mgModSummaries module_graph
    -- stronglyConnCompG flips the original order, so if we reverse
    -- the summaries we get a stable topological sort.
    (graph, lookup_node) =
      moduleGraphNodes drop_hs_boot_nodes (reverse summaries)

    initial_graph = case mb_root_mod of
        Nothing -> graph
        Just root_mod ->
            -- restrict the graph to just those modules reachable from
            -- the specified module.  We do this by building a graph with
            -- the full set of nodes, and determining the reachable set from
            -- the specified node.
            let root | Just node <- lookup_node HsSrcFile root_mod
                     , graph `hasVertexG` node
                     = node
                     | otherwise
                     = throwGhcException (ProgramError "module does not exist")
            in graphFromEdgedVerticesUniq (seq root (reachableG graph root))

type SummaryNode = Node Int ModSummary

summaryNodeKey :: SummaryNode -> Int
summaryNodeKey = node_key

summaryNodeSummary :: SummaryNode -> ModSummary
summaryNodeSummary = node_payload

moduleGraphNodes :: Bool -> [ModSummary]
  -> (Graph SummaryNode, HscSource -> ModuleName -> Maybe SummaryNode)
moduleGraphNodes drop_hs_boot_nodes summaries =
  (graphFromEdgedVerticesUniq nodes, lookup_node)
  where
    numbered_summaries = zip summaries [1..]

    lookup_node :: HscSource -> ModuleName -> Maybe SummaryNode
    lookup_node hs_src mod = Map.lookup (mod, hscSourceToIsBoot hs_src) node_map

    lookup_key :: HscSource -> ModuleName -> Maybe Int
    lookup_key hs_src mod = fmap summaryNodeKey (lookup_node hs_src mod)

    node_map :: NodeMap SummaryNode
    node_map = Map.fromList [ ((moduleName (ms_mod s),
                                hscSourceToIsBoot (ms_hsc_src s)), node)
                            | node <- nodes
                            , let s = summaryNodeSummary node ]

    -- We use integers as the keys for the SCC algorithm
    nodes :: [SummaryNode]
    nodes = [ DigraphNode s key out_keys
            | (s, key) <- numbered_summaries
             -- Drop the hi-boot ones if told to do so
            , not (isBootSummary s && drop_hs_boot_nodes)
            , let out_keys = out_edge_keys hs_boot_key (map unLoc (ms_home_srcimps s)) ++
                             out_edge_keys HsSrcFile   (map unLoc (ms_home_imps s)) ++
                             (-- see [boot-edges] below
                              if drop_hs_boot_nodes || ms_hsc_src s == HsBootFile
                              then []
                              else case lookup_key HsBootFile (ms_mod_name s) of
                                    Nothing -> []
                                    Just k  -> [k]) ]

    -- [boot-edges] if this is a .hs and there is an equivalent
    -- .hs-boot, add a link from the former to the latter.  This
    -- has the effect of detecting bogus cases where the .hs-boot
    -- depends on the .hs, by introducing a cycle.  Additionally,
    -- it ensures that we will always process the .hs-boot before
    -- the .hs, and so the HomePackageTable will always have the
    -- most up to date information.

    -- Drop hs-boot nodes by using HsSrcFile as the key
    hs_boot_key | drop_hs_boot_nodes = HsSrcFile
                | otherwise          = HsBootFile

    out_edge_keys :: HscSource -> [ModuleName] -> [Int]
    out_edge_keys hi_boot ms = mapMaybe (lookup_key hi_boot) ms
        -- If we want keep_hi_boot_nodes, then we do lookup_key with
        -- IsBoot; else NotBoot

-- The nodes of the graph are keyed by (mod, is boot?) pairs
-- NB: hsig files show up as *normal* nodes (not boot!), since they don't
-- participate in cycles (for now)
type NodeKey   = (ModuleName, IsBoot)
type NodeMap a = Map.Map NodeKey a

msKey :: ModSummary -> NodeKey
msKey (ModSummary { ms_mod = mod, ms_hsc_src = boot })
    = (moduleName mod, hscSourceToIsBoot boot)

mkNodeMap :: [ModSummary] -> NodeMap ModSummary
mkNodeMap summaries = Map.fromList [ (msKey s, s) | s <- summaries]

nodeMapElts :: NodeMap a -> [a]
nodeMapElts = Map.elems

-- | If there are {-# SOURCE #-} imports between strongly connected
-- components in the topological sort, then those imports can
-- definitely be replaced by ordinary non-SOURCE imports: if SOURCE
-- were necessary, then the edge would be part of a cycle.
warnUnnecessarySourceImports :: GhcMonad m => [SCC ModSummary] -> m ()
warnUnnecessarySourceImports sccs = do
  dflags <- getDynFlags
  when (wopt Opt_WarnUnusedImports dflags)
    (logWarnings (listToBag (concatMap (check dflags . flattenSCC) sccs)))
  where check dflags ms =
           let mods_in_this_cycle = map ms_mod_name ms in
           [ warn dflags i | m <- ms, i <- ms_home_srcimps m,
                             unLoc i `notElem`  mods_in_this_cycle ]

        warn :: DynFlags -> Located ModuleName -> WarnMsg
        warn dflags (L loc mod) =
           mkPlainErrMsg dflags loc
                (text "Warning: {-# SOURCE #-} unnecessary in import of "
                 <+> quotes (ppr mod))


reportImportErrors :: MonadIO m => [Either ErrorMessages b] -> m [b]
reportImportErrors xs | null errs = return oks
                      | otherwise = throwErrors $ unionManyBags errs
  where (errs, oks) = partitionEithers xs


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
          -> [ModSummary]       -- Old summaries
          -> [ModuleName]       -- Ignore dependencies on these; treat
                                -- them as if they were package modules
          -> Bool               -- True <=> allow multiple targets to have
                                --          the same module name; this is
                                --          very useful for ghc -M
          -> IO [Either ErrorMessages ModSummary]
                -- The elts of [ModSummary] all have distinct
                -- (Modules, IsBoot) identifiers, unless the Bool is true
                -- in which case there can be repeats
downsweep hsc_env old_summaries excl_mods allow_dup_roots
   = do
       rootSummaries <- mapM getRootSummary roots
       rootSummariesOk <- reportImportErrors rootSummaries
       let root_map = mkRootMap rootSummariesOk
       checkDuplicates root_map
       map0 <- loop (concatMap calcDeps rootSummariesOk) root_map
       -- if we have been passed -fno-code, we enable code generation
       -- for dependencies of modules that have -XTemplateHaskell,
       -- otherwise those modules will fail to compile.
       -- See Note [-fno-code mode] #8025
       map1 <- if hscTarget dflags == HscNothing
         then enableCodeGenForTH
           (defaultObjectTarget dflags)
           map0
         else if hscTarget dflags == HscInterpreted
           then enableCodeGenForUnboxedTuplesOrSums
             (defaultObjectTarget dflags)
             map0
           else return map0
       return $ concat $ nodeMapElts map1
     where
        calcDeps = msDeps

        dflags = hsc_dflags hsc_env
        roots = hsc_targets hsc_env

        old_summary_map :: NodeMap ModSummary
        old_summary_map = mkNodeMap old_summaries

        getRootSummary :: Target -> IO (Either ErrorMessages ModSummary)
        getRootSummary (Target (TargetFile file mb_phase) obj_allowed maybe_buf)
           = do exists <- liftIO $ doesFileExist file
                if exists || isJust maybe_buf
                    then summariseFile hsc_env old_summaries file mb_phase
                                       obj_allowed maybe_buf
                    else return $ Left $ unitBag $ mkPlainErrMsg dflags noSrcSpan $
                           text "can't find file:" <+> text file
        getRootSummary (Target (TargetModule modl) obj_allowed maybe_buf)
           = do maybe_summary <- summariseModule hsc_env old_summary_map NotBoot
                                           (L rootLoc modl) obj_allowed
                                           maybe_buf excl_mods
                case maybe_summary of
                   Nothing -> return $ Left $ moduleNotFoundErr dflags modl
                   Just s  -> return s

        rootLoc = mkGeneralSrcSpan (fsLit "<command line>")

        -- In a root module, the filename is allowed to diverge from the module
        -- name, so we have to check that there aren't multiple root files
        -- defining the same module (otherwise the duplicates will be silently
        -- ignored, leading to confusing behaviour).
        checkDuplicates :: NodeMap [Either ErrorMessages ModSummary] -> IO ()
        checkDuplicates root_map
           | allow_dup_roots = return ()
           | null dup_roots  = return ()
           | otherwise       = liftIO $ multiRootsErr dflags (head dup_roots)
           where
             dup_roots :: [[ModSummary]]        -- Each at least of length 2
             dup_roots = filterOut isSingleton $ map rights $ nodeMapElts root_map

        loop :: [(Located ModuleName,IsBoot)]
                        -- Work list: process these modules
             -> NodeMap [Either ErrorMessages ModSummary]
                        -- Visited set; the range is a list because
                        -- the roots can have the same module names
                        -- if allow_dup_roots is True
             -> IO (NodeMap [Either ErrorMessages ModSummary])
                        -- The result is the completed NodeMap
        loop [] done = return done
        loop ((wanted_mod, is_boot) : ss) done
          | Just summs <- Map.lookup key done
          = if isSingleton summs then
                loop ss done
            else
                do { multiRootsErr dflags (rights summs); return Map.empty }
          | otherwise
          = do mb_s <- summariseModule hsc_env old_summary_map
                                       is_boot wanted_mod True
                                       Nothing excl_mods
               case mb_s of
                   Nothing -> loop ss done
                   Just (Left e) -> loop ss (Map.insert key [Left e] done)
                   Just (Right s)-> do
                     new_map <-
                       loop (calcDeps s) (Map.insert key [Right s] done)
                     loop ss new_map
          where
            key = (unLoc wanted_mod, is_boot)

-- | Update the every ModSummary that is depended on
-- by a module that needs template haskell. We enable codegen to
-- the specified target, disable optimization and change the .hi
-- and .o file locations to be temporary files.
-- See Note [-fno-code mode]
enableCodeGenForTH :: HscTarget
  -> NodeMap [Either ErrorMessages ModSummary]
  -> IO (NodeMap [Either ErrorMessages ModSummary])
enableCodeGenForTH =
  enableCodeGenWhen condition should_modify TFL_CurrentModule TFL_GhcSession
  where
    condition = isTemplateHaskellOrQQNonBoot
    should_modify (ModSummary { ms_hspp_opts = dflags }) =
      hscTarget dflags == HscNothing &&
      -- Don't enable codegen for TH on indefinite packages; we
      -- can't compile anything anyway! See #16219.
      not (isIndefinite dflags)

-- | Update the every ModSummary that is depended on
-- by a module that needs unboxed tuples. We enable codegen to
-- the specified target, disable optimization and change the .hi
-- and .o file locations to be temporary files.
--
-- This is used used in order to load code that uses unboxed tuples
-- or sums into GHCi while still allowing some code to be interpreted.
enableCodeGenForUnboxedTuplesOrSums :: HscTarget
  -> NodeMap [Either ErrorMessages ModSummary]
  -> IO (NodeMap [Either ErrorMessages ModSummary])
enableCodeGenForUnboxedTuplesOrSums =
  enableCodeGenWhen condition should_modify TFL_GhcSession TFL_CurrentModule
  where
    condition ms =
      unboxed_tuples_or_sums (ms_hspp_opts ms) &&
      not (gopt Opt_ByteCode (ms_hspp_opts ms)) &&
      not (isBootSummary ms)
    unboxed_tuples_or_sums d =
      xopt LangExt.UnboxedTuples d || xopt LangExt.UnboxedSums d
    should_modify (ModSummary { ms_hspp_opts = dflags }) =
      hscTarget dflags == HscInterpreted

-- | Helper used to implement 'enableCodeGenForTH' and
-- 'enableCodeGenForUnboxedTuples'. In particular, this enables
-- unoptimized code generation for all modules that meet some
-- condition (first parameter), or are dependencies of those
-- modules. The second parameter is a condition to check before
-- marking modules for code generation.
enableCodeGenWhen
  :: (ModSummary -> Bool)
  -> (ModSummary -> Bool)
  -> TempFileLifetime
  -> TempFileLifetime
  -> HscTarget
  -> NodeMap [Either ErrorMessages ModSummary]
  -> IO (NodeMap [Either ErrorMessages ModSummary])
enableCodeGenWhen condition should_modify staticLife dynLife target nodemap =
  traverse (traverse (traverse enable_code_gen)) nodemap
  where
    enable_code_gen ms
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
              tn <- newTempName dflags staticLife suf
              let dyn_tn = tn -<.> dynsuf
              addFilesToClean dflags dynLife [dyn_tn]
              return tn
          -- We don't want to create .o or .hi files unless we have been asked
          -- to by the user. But we need them, so we patch their locations in
          -- the ModSummary with temporary files.
          --
        (hi_file, o_file) <-
          -- If ``-fwrite-interface` is specified, then the .o and .hi files
          -- are written into `-odir` and `-hidir` respectively.  #16670
          if gopt Opt_WriteInterface dflags
            then return (ml_hi_file ms_location, ml_obj_file ms_location)
            else (,) <$> (new_temp_file (hiSuf dflags) (dynHiSuf dflags))
                     <*> (new_temp_file (objectSuf dflags) (dynObjectSuf dflags))
        return $
          ms
          { ms_location =
              ms_location {ml_hi_file = hi_file, ml_obj_file = o_file}
          , ms_hspp_opts = updOptLevel 0 $ dflags {hscTarget = target}
          }
      | otherwise = return ms

    needs_codegen_set = transitive_deps_set
      [ ms
      | mss <- Map.elems nodemap
      , Right ms <- mss
      , condition ms
      ]

    -- find the set of all transitive dependencies of a list of modules.
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
                  | (L _ mn, NotBoot) <- msDeps ms
                  , dep_ms <-
                      toList (Map.lookup (mn, NotBoot) nodemap) >>= toList >>=
                      toList
                  ]
                new_marked_mods = Set.insert ms_mod marked_mods
            in foldl' go new_marked_mods deps

mkRootMap :: [ModSummary] -> NodeMap [Either ErrorMessages ModSummary]
mkRootMap summaries = Map.insertListWith (flip (++))
                                         [ (msKey s, [Right s]) | s <- summaries ]
                                         Map.empty

-- | Returns the dependencies of the ModSummary s.
-- A wrinkle is that for a {-# SOURCE #-} import we return
--      *both* the hs-boot file
--      *and* the source file
-- as "dependencies".  That ensures that the list of all relevant
-- modules always contains B.hs if it contains B.hs-boot.
-- Remember, this pass isn't doing the topological sort.  It's
-- just gathering the list of all relevant ModSummaries
msDeps :: ModSummary -> [(Located ModuleName, IsBoot)]
msDeps s =
    concat [ [(m,IsBoot), (m,NotBoot)] | m <- ms_home_srcimps s ]
        ++ [ (m,NotBoot) | m <- ms_home_imps s ]

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
        -> [ModSummary]                 -- old summaries
        -> FilePath                     -- source file name
        -> Maybe Phase                  -- start phase
        -> Bool                         -- object code allowed?
        -> Maybe (StringBuffer,UTCTime)
        -> IO (Either ErrorMessages ModSummary)

summariseFile hsc_env old_summaries src_fn mb_phase obj_allowed maybe_buf
        -- we can use a cached summary if one is available and the
        -- source file hasn't changed,  But we have to look up the summary
        -- by source file, rather than module name as we do in summarise.
   | Just old_summary <- findSummaryBySourceFile old_summaries src_fn
   = do
        let location = ms_location old_summary
            dflags = hsc_dflags hsc_env

        src_timestamp <- get_src_timestamp
                -- The file exists; we checked in getRootSummary above.
                -- If it gets removed subsequently, then this
                -- getModificationUTCTime may fail, but that's the right
                -- behaviour.

                -- return the cached summary if the source didn't change
        checkSummaryTimestamp
            hsc_env dflags obj_allowed NotBoot (new_summary src_fn)
            old_summary location src_timestamp

   | otherwise
   = do src_timestamp <- get_src_timestamp
        new_summary src_fn src_timestamp
  where
    get_src_timestamp = case maybe_buf of
                           Just (_,t) -> return t
                           Nothing    -> liftIO $ getModificationUTCTime src_fn
                        -- getModificationUTCTime may fail

    new_summary src_fn src_timestamp = runExceptT $ do
        preimps@PreprocessedImports {..}
            <- getPreprocessedImports hsc_env src_fn mb_phase maybe_buf


        -- Make a ModLocation for this file
        location <- liftIO $ mkHomeModLocation (hsc_dflags hsc_env) pi_mod_name src_fn

        -- Tell the Finder cache where it is, so that subsequent calls
        -- to findModule will find it, even if it's not on any search path
        mod <- liftIO $ addHomeModuleToFinder hsc_env pi_mod_name location

        liftIO $ makeNewModSummary hsc_env $ MakeNewModSummary
            { nms_src_fn = src_fn
            , nms_src_timestamp = src_timestamp
            , nms_is_boot = NotBoot
            , nms_hsc_src =
                if isHaskellSigFilename src_fn
                   then HsigFile
                   else HsSrcFile
            , nms_location = location
            , nms_mod = mod
            , nms_obj_allowed = obj_allowed
            , nms_preimps = preimps
            }

findSummaryBySourceFile :: [ModSummary] -> FilePath -> Maybe ModSummary
findSummaryBySourceFile summaries file
  = case [ ms | ms <- summaries, HsSrcFile <- [ms_hsc_src ms],
                                 expectJust "findSummaryBySourceFile" (ml_hs_file (ms_location ms)) == file ] of
        [] -> Nothing
        (x:_) -> Just x

checkSummaryTimestamp
    :: HscEnv -> DynFlags -> Bool -> IsBoot
    -> (UTCTime -> IO (Either e ModSummary))
    -> ModSummary -> ModLocation -> UTCTime
    -> IO (Either e ModSummary)
checkSummaryTimestamp
  hsc_env dflags obj_allowed is_boot new_summary
  old_summary location src_timestamp
  | ms_hs_date old_summary == src_timestamp &&
      not (gopt Opt_ForceRecomp (hsc_dflags hsc_env)) = do
           -- update the object-file timestamp
           obj_timestamp <-
             if isObjectTarget (hscTarget (hsc_dflags hsc_env))
                 || obj_allowed -- bug #1205
                 then liftIO $ getObjTimestamp location is_boot
                 else return Nothing

           -- We have to repopulate the Finder's cache for file targets
           -- because the file might not even be on the regular search path
           -- and it was likely flushed in depanal. This is not technically
           -- needed when we're called from sumariseModule but it shouldn't
           -- hurt.
           _ <- addHomeModuleToFinder hsc_env
                  (moduleName (ms_mod old_summary)) location

           hi_timestamp <- maybeGetIfaceDate dflags location
           hie_timestamp <- modificationTimeIfExists (ml_hie_file location)

           return $ Right old_summary
               { ms_obj_date = obj_timestamp
               , ms_iface_date = hi_timestamp
               , ms_hie_date = hie_timestamp
               }

   | otherwise =
           -- source changed: re-summarise.
           new_summary src_timestamp

-- Summarise a module, and pick up source and timestamp.
summariseModule
          :: HscEnv
          -> NodeMap ModSummary -- Map of old summaries
          -> IsBoot             -- IsBoot <=> a {-# SOURCE #-} import
          -> Located ModuleName -- Imported module to be summarised
          -> Bool               -- object code allowed?
          -> Maybe (StringBuffer, UTCTime)
          -> [ModuleName]               -- Modules to exclude
          -> IO (Maybe (Either ErrorMessages ModSummary))      -- Its new summary

summariseModule hsc_env old_summary_map is_boot (L loc wanted_mod)
                obj_allowed maybe_buf excl_mods
  | wanted_mod `elem` excl_mods
  = return Nothing

  | Just old_summary <- Map.lookup (wanted_mod, is_boot) old_summary_map
  = do          -- Find its new timestamp; all the
                -- ModSummaries in the old map have valid ml_hs_files
        let location = ms_location old_summary
            src_fn = expectJust "summariseModule" (ml_hs_file location)

                -- check the modification time on the source file, and
                -- return the cached summary if it hasn't changed.  If the
                -- file has disappeared, we need to call the Finder again.
        case maybe_buf of
           Just (_,t) ->
               Just <$> check_timestamp old_summary location src_fn t
           Nothing    -> do
                m <- tryIO (getModificationUTCTime src_fn)
                case m of
                   Right t ->
                       Just <$> check_timestamp old_summary location src_fn t
                   Left e | isDoesNotExistError e -> find_it
                          | otherwise             -> ioError e

  | otherwise  = find_it
  where
    dflags = hsc_dflags hsc_env

    check_timestamp old_summary location src_fn =
        checkSummaryTimestamp
          hsc_env dflags obj_allowed is_boot
          (new_summary location (ms_mod old_summary) src_fn)
          old_summary location

    find_it = do
        found <- findImportedModule hsc_env wanted_mod Nothing
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
        let location' | IsBoot <- is_boot = addBootSuffixLocn location
                      | otherwise         = location
            src_fn = expectJust "summarise2" (ml_hs_file location')

                -- Check that it exists
                -- It might have been deleted since the Finder last found it
        maybe_t <- modificationTimeIfExists src_fn
        case maybe_t of
          Nothing -> return $ Left $ noHsFileErr dflags loc src_fn
          Just t  -> new_summary location' mod src_fn t

    new_summary location mod src_fn src_timestamp
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
        let hsc_src = case is_boot of
                IsBoot -> HsBootFile
                _ | isHaskellSigFilename src_fn -> HsigFile
                  | otherwise -> HsSrcFile

        when (pi_mod_name /= wanted_mod) $
                throwE $ unitBag $ mkPlainErrMsg pi_local_dflags pi_mod_name_loc $
                              text "File name does not match module name:"
                              $$ text "Saw:" <+> quotes (ppr pi_mod_name)
                              $$ text "Expected:" <+> quotes (ppr wanted_mod)

        when (hsc_src == HsigFile && isNothing (lookup pi_mod_name (thisUnitIdInsts dflags))) $
            let suggested_instantiated_with =
                    hcat (punctuate comma $
                        [ ppr k <> text "=" <> ppr v
                        | (k,v) <- ((pi_mod_name, mkHoleModule pi_mod_name)
                                : thisUnitIdInsts dflags)
                        ])
            in throwE $ unitBag $ mkPlainErrMsg pi_local_dflags pi_mod_name_loc $
                text "Unexpected signature:" <+> quotes (ppr pi_mod_name)
                $$ if gopt Opt_BuildingCabalPackage dflags
                    then parens (text "Try adding" <+> quotes (ppr pi_mod_name)
                            <+> text "to the"
                            <+> quotes (text "signatures")
                            <+> text "field in your Cabal file.")
                    else parens (text "Try passing -instantiated-with=\"" <>
                                 suggested_instantiated_with <> text "\"" $$
                                text "replacing <" <> ppr pi_mod_name <> text "> as necessary.")

        liftIO $ makeNewModSummary hsc_env $ MakeNewModSummary
            { nms_src_fn = src_fn
            , nms_src_timestamp = src_timestamp
            , nms_is_boot = is_boot
            , nms_hsc_src = hsc_src
            , nms_location = location
            , nms_mod = mod
            , nms_obj_allowed = obj_allowed
            , nms_preimps = preimps
            }

-- | Convenience named arguments for 'makeNewModSummary' only used to make
-- code more readable, not exported.
data MakeNewModSummary
  = MakeNewModSummary
      { nms_src_fn :: FilePath
      , nms_src_timestamp :: UTCTime
      , nms_is_boot :: IsBoot
      , nms_hsc_src :: HscSource
      , nms_location :: ModLocation
      , nms_mod :: Module
      , nms_obj_allowed :: Bool
      , nms_preimps :: PreprocessedImports
      }

makeNewModSummary :: HscEnv -> MakeNewModSummary -> IO ModSummary
makeNewModSummary hsc_env MakeNewModSummary{..} = do
  let PreprocessedImports{..} = nms_preimps
  let dflags = hsc_dflags hsc_env

  -- when the user asks to load a source file by name, we only
  -- use an object file if -fobject-code is on.  See #1205.
  obj_timestamp <- liftIO $
      if isObjectTarget (hscTarget dflags)
         || nms_obj_allowed -- bug #1205
          then getObjTimestamp nms_location nms_is_boot
          else return Nothing

  hi_timestamp <- maybeGetIfaceDate dflags nms_location
  hie_timestamp <- modificationTimeIfExists (ml_hie_file nms_location)

  extra_sig_imports <- findExtraSigImports hsc_env nms_hsc_src pi_mod_name
  required_by_imports <- implicitRequirements hsc_env pi_theimps

  return $ ModSummary
      { ms_mod = nms_mod
      , ms_hsc_src = nms_hsc_src
      , ms_location = nms_location
      , ms_hspp_file = pi_hspp_fn
      , ms_hspp_opts = pi_local_dflags
      , ms_hspp_buf  = Just pi_hspp_buf
      , ms_parsed_mod = Nothing
      , ms_srcimps = pi_srcimps
      , ms_textual_imps =
          pi_theimps ++ extra_sig_imports ++ required_by_imports
      , ms_hs_date = nms_src_timestamp
      , ms_iface_date = hi_timestamp
      , ms_hie_date = hie_timestamp
      , ms_obj_date = obj_timestamp
      }

getObjTimestamp :: ModLocation -> IsBoot -> IO (Maybe UTCTime)
getObjTimestamp location is_boot
  = if is_boot == IsBoot then return Nothing
                         else modificationTimeIfExists (ml_obj_file location)

data PreprocessedImports
  = PreprocessedImports
      { pi_local_dflags :: DynFlags
      , pi_srcimps  :: [(Maybe FastString, Located ModuleName)]
      , pi_theimps  :: [(Maybe FastString, Located ModuleName)]
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
    -> ExceptT ErrorMessages IO PreprocessedImports
getPreprocessedImports hsc_env src_fn mb_phase maybe_buf = do
  (pi_local_dflags, pi_hspp_fn)
      <- ExceptT $ preprocess hsc_env src_fn (fst <$> maybe_buf) mb_phase
  pi_hspp_buf <- liftIO $ hGetStringBuffer pi_hspp_fn
  (pi_srcimps, pi_theimps, L pi_mod_name_loc pi_mod_name)
      <- ExceptT $ getImports pi_local_dflags pi_hspp_buf pi_hspp_fn src_fn
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

    let deferDiagnostics _dflags !reason !severity !srcSpan !style !msg = do
          let action = putLogMsg dflags reason severity srcSpan style msg
          case severity of
            SevWarning -> atomicModifyIORef' warnings $ \i -> (action: i, ())
            SevError -> atomicModifyIORef' errors $ \i -> (action: i, ())
            SevFatal -> atomicModifyIORef' fatals $ \i -> (action: i, ())
            _ -> action

        printDeferredDiagnostics = liftIO $
          forM_ [warnings, errors, fatals] $ \ref -> do
            -- This IORef can leak when the dflags leaks, so let us always
            -- reset the content.
            actions <- atomicModifyIORef' ref $ \i -> ([], i)
            sequence_ $ reverse actions

        setLogAction action = modifySession $ \hsc_env ->
          hsc_env{ hsc_dflags = (hsc_dflags hsc_env){ log_action = action } }

    gbracket
      (setLogAction deferDiagnostics)
      (\_ -> setLogAction (log_action dflags) >> printDeferredDiagnostics)
      (\_ -> f)

noModError :: DynFlags -> SrcSpan -> ModuleName -> FindResult -> ErrMsg
-- ToDo: we don't have a proper line number for this error
noModError dflags loc wanted_mod err
  = mkPlainErrMsg dflags loc $ cannotFindModule dflags wanted_mod err

noHsFileErr :: DynFlags -> SrcSpan -> String -> ErrorMessages
noHsFileErr dflags loc path
  = unitBag $ mkPlainErrMsg dflags loc $ text "Can't find" <+> text path

moduleNotFoundErr :: DynFlags -> ModuleName -> ErrorMessages
moduleNotFoundErr dflags mod
  = unitBag $ mkPlainErrMsg dflags noSrcSpan $
        text "module" <+> quotes (ppr mod) <+> text "cannot be found locally"

multiRootsErr :: DynFlags -> [ModSummary] -> IO ()
multiRootsErr _      [] = panic "multiRootsErr"
multiRootsErr dflags summs@(summ1:_)
  = throwOneError $ mkPlainErrMsg dflags noSrcSpan $
        text "module" <+> quotes (ppr mod) <+>
        text "is defined in multiple files:" <+>
        sep (map text files)
  where
    mod = ms_mod summ1
    files = map (expectJust "checkDup" . ml_hs_file . ms_location) summs

keepGoingPruneErr :: [ModuleName] -> SDoc
keepGoingPruneErr ms
  = vcat (( text "-fkeep-going in use, removing the following" <+>
            text "dependencies and continuing:"):
          map (nest 6 . ppr) ms )

cyclicModuleErr :: [ModSummary] -> SDoc
-- From a strongly connected component we find
-- a single cycle to report
cyclicModuleErr mss
  = ASSERT( not (null mss) )
    case findCycle graph of
       Nothing   -> text "Unexpected non-cycle" <+> ppr mss
       Just path -> vcat [ text "Module imports form a cycle:"
                         , nest 2 (show_path path) ]
  where
    graph :: [Node NodeKey ModSummary]
    graph = [ DigraphNode ms (msKey ms) (get_deps ms) | ms <- mss]

    get_deps :: ModSummary -> [NodeKey]
    get_deps ms = ([ (unLoc m, IsBoot)  | m <- ms_home_srcimps ms ] ++
                   [ (unLoc m, NotBoot) | m <- ms_home_imps    ms ])

    show_path []         = panic "show_path"
    show_path [m]        = text "module" <+> ppr_ms m
                           <+> text "imports itself"
    show_path (m1:m2:ms) = vcat ( nest 7 (text "module" <+> ppr_ms m1)
                                : nest 6 (text "imports" <+> ppr_ms m2)
                                : go ms )
       where
         go []     = [text "which imports" <+> ppr_ms m1]
         go (m:ms) = (text "which imports" <+> ppr_ms m) : go ms


    ppr_ms :: ModSummary -> SDoc
    ppr_ms ms = quotes (ppr (moduleName (ms_mod ms))) <+>
                (parens (text (msHsFilePath ms)))
