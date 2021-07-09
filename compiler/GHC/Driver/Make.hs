{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleContexts #-}

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
        load, load', LoadHowMuch(..),
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
        IsBootInterface(..),

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

import GHC.Driver.Config.Logger (initLogFlags)
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
import GHC.Driver.Graph

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

import GHC.Utils.Exception ( AsyncException(..), evaluate )
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
import GHC.Types.Unique.DFM
import GHC.Types.Unique.FM
import GHC.Types.Unique.DSet
import GHC.Types.Unique.Set
import GHC.Types.Name
import GHC.Types.Name.Env

import GHC.Unit
import GHC.Unit.External
import GHC.Unit.Finder
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.Graph
import GHC.Unit.Home.ModInfo

import Data.Either ( rights, partitionEithers )
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC.Data.FiniteMap as Map ( insertListWith )

import Control.Concurrent ( forkIOWithUnmask, killThread )
import qualified GHC.Conc as CC
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Monad
import Control.Monad.Trans.Except ( ExceptT(..), runExceptT, throwE )
import qualified Control.Monad.Catch as MC
import Data.IORef
import Data.List (nub, sort, partition)
import Data.Foldable (toList)
import Data.Maybe
import Data.Time
import Data.Bifunctor (first)
import System.Directory
import System.FilePath
import System.IO        ( fixIO )

import GHC.Conc ( getNumProcessors, getNumCapabilities, setNumCapabilities )

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
        warnMissingHomeModules hsc_env mod_graph
        setSession hsc_env { hsc_mod_graph = mod_graph }
        pure (errs, mod_graph)
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
        fmap ModuleNode mod_summaries ++ instantiationNodes (hsc_units hsc_env)
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
warnMissingHomeModules :: GhcMonad m => HscEnv -> ModuleGraph -> m ()
warnMissingHomeModules hsc_env mod_graph =
    when (not (null missing)) $
        logDiagnostics (GhcDriverMessage <$> warn)
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
--
load :: GhcMonad m => LoadHowMuch -> m SuccessFlag
load how_much = do
    (errs, mod_graph) <- depanalE [] False                        -- #17459
    success <- load' how_much (Just batchMsg) mod_graph
    warnUnusedPackages
    if isEmptyMessages errs
      then pure success
      else throwErrors (fmap GhcDriverMessage errs)

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
        state  = hsc_units  hsc_env
        pit = eps_PIT eps
        diag_opts = initDiagOpts dflags

    let loadedPackages
          = map (unsafeLookupUnit state)
          . nub . sort
          . map moduleUnit
          . moduleEnvKeys
          $ pit

        requestedArgs = mapMaybe packageArg (packageFlags dflags)

        unusedArgs
          = filter (\arg -> not $ any (matching state arg) loadedPackages)
                   requestedArgs

    let warn = singleMessage $ mkPlainMsgEnvelope diag_opts noSrcSpan (DriverUnusedPackages unusedArgs)

    when (not (null unusedArgs)) $
      logDiagnostics (GhcDriverMessage <$> warn)

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
                    return Failed

    checkHowMuch how_much $ do

    -- mg2_with_srcimps drops the hi-boot nodes, returning a
    -- graph with cycles.  Among other things, it is used for
    -- backing out partially complete cycles following a failed
    -- upsweep, and for removing from hpt all the modules
    -- not in strict downwards closure, during calls to compile.
    let mg2_with_srcimps :: [SCC ModSummary]
        mg2_with_srcimps = filterToposortToModules $
          topSortModuleGraph True mod_graph Nothing

    -- If we can determine that any of the {-# SOURCE #-} imports
    -- are definitely unnecessary, then emit a warning.
    warnUnnecessarySourceImports mg2_with_srcimps


    let
        -- prune the HPT so everything is not retained when doing an
        -- upsweep.
        pruned_hpt = pruneHomePackageTable hpt1
                            (flattenSCCs mg2_with_srcimps)

    _ <- liftIO $ evaluate pruned_hpt

    -- before we unload anything, make sure we don't leave an old
    -- interactive context around pointing to dead bindings.  Also,
    -- write the pruned HPT to allow the old HPT to be GC'd.
    setSession $ discardIC $ hscUpdateHPT (const pruned_hpt) hsc_env

    -- Unload everything
    liftIO $ unload interp hsc_env


    -- We could at this point detect cycles which aren't broken by
    -- a source-import, and complain immediately, but it seems better
    -- to let upsweep_mods do this, so at least some useful work gets
    -- done before the upsweep is abandoned.
    --hPutStrLn stderr "after tsort:\n"
    --hPutStrLn stderr (showSDoc (vcat (map ppr mg2)))

    -- Now do the upsweep, calling compile for each module in
    -- turn.  Final result is version 3 of everything.

    setSession $ hscUpdateHPT (const emptyHomePackageTable) hsc_env
    (upsweep_ok, modsUpswept) <- withDeferredDiagnostics $
      upsweep_mods mHscMessage pruned_hpt how_much mod_graph

    -- Make modsDone be the summaries for each home module now
    -- available; this should equal the domain of hpt3.
    -- Get in in a roughly top .. bottom order (hence reverse).

    let nodesDone = reverse modsUpswept
        (_, modsDone) = partitionNodes nodesDone

    -- Try and do linking in some form, depending on whether the
    -- upsweep was completely or only partially successful.

    if succeeded upsweep_ok

     then
       -- Easy; just relink it all.
       do liftIO $ debugTraceMsg logger 2 (text "Upsweep completely successful.")

          -- Clean up after ourselves
          hsc_env1 <- getSession
          liftIO $ cleanCurrentModuleTempFiles logger (hsc_tmpfs hsc_env1) dflags

          -- Issue a warning for the confusing case where the user
          -- said '-o foo' but we're not going to do any linking.
          -- We attempt linking if either (a) one of the modules is
          -- called Main, or (b) the user said -no-hs-main, indicating
          -- that main() is going to come from somewhere else.
          --
          let ofile = outputFile dflags
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

     else
       -- Tricky.  We need to back out the effects of compiling any
       -- half-done cycles, both so as to clean up the top level envs
       -- and to avoid telling the interactive linker to link them.
       do liftIO $ debugTraceMsg logger 2 (text "Upsweep partially successful.")

          let modsDone_names
                 = map (ms_mod . emsModSummary) modsDone
          let mods_to_zap_names
                 = findPartiallyCompletedCycles modsDone_names
                      mg2_with_srcimps
          let (mods_to_clean, mods_to_keep) =
                partition ((`Set.member` mods_to_zap_names).ms_mod) $
                emsModSummary <$> modsDone
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
          tmpfs <- hsc_tmpfs <$> getSession
          liftIO $ changeTempFilesLifetime tmpfs TFL_CurrentModule unneeded_temps
          liftIO $ cleanCurrentModuleTempFiles logger tmpfs dflags

          let hpt5 = retainInTopLevelEnvs (map ms_mod_name mods_to_keep)
                                          hpt4

          -- Clean up after ourselves

          -- there should be no Nothings where linkables should be, now
          let just_linkables =
                    isNoLink (ghcLink dflags)
                 || allHpt (isJust.hm_linkable)
                        (filterHpt ((== HsSrcFile).mi_hsc_src.hm_iface)
                                hpt5)
          assert just_linkables $ do

          -- Link everything together
          hsc_env <- getSession
          linkresult <- liftIO $ link (ghcLink dflags)
                                      logger
                                      (hsc_tmpfs hsc_env)
                                      (hsc_hooks hsc_env)
                                      dflags
                                      (hsc_unit_env hsc_env)
                                      False
                                      hpt5

          modifySession $ hscUpdateHPT (const hpt5)
          loadFinish Failed linkresult

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

        name_exe = do
          -- we must add the .exe extension unconditionally here, otherwise
          -- when name has an extension of its own, the .exe extension will
          -- not be added by GHC.Driver.Pipeline.exeFileName.  See #2248
          name' <- if platformOS platform == OSMinGW32
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
pruneHomePackageTable :: HomePackageTable
                      -> [ModSummary]
                      -> HomePackageTable
pruneHomePackageTable hpt summ
  = mapHpt prune hpt
  where prune hmi = hmi'{ hm_details = emptyModDetails }
          where
           modl = moduleName (mi_module (hm_iface hmi))
           hmi' | mi_src_hash (hm_iface hmi) /= ms_hs_hash ms
                = hmi{ hm_linkable = Nothing }
                | otherwise
                = hmi
                where ms = expectJust "prune" (lookupUFM ms_map modl)

        ms_map = listToUFM [(ms_mod_name ms, ms) | ms <- summ]


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
unload :: Interp -> HscEnv -> IO ()
unload interp hsc_env
  = case ghcLink (hsc_dflags hsc_env) of
        LinkInMemory -> Linker.unload interp hsc_env []
        _other -> return ()

-- -----------------------------------------------------------------------------
{- |

  Stability tells us which modules definitely do not need to be recompiled.
  There are two main reasons for having stability:

   - avoid doing a complete upsweep of the module graph in GHCi when
     modules near the bottom of the tree have not changed.

   - to tell GHCi when it can load object code: we can only load object code
     for a module when we also load object code for all of the imports of the
     module.  So we need to know that we will definitely not be recompiling
     any of these modules, and we can use the object code.

  The stability check is as follows.  Both stableObject and
  stableBCO are used during the upsweep phase later.

@
  stable m = stableObject m || stableBCO m

  stableObject m =
        all stableObject (imports m)
        && old linkable does not exist, or is == on-disk .o
        && date(on-disk .o) >= date(on-disk .hi)
        && hash(on-disk .hs) == hash recorded in .hi

  stableBCO m =
        all stable (imports m)
        && hash(on-disk .hs) == hash recorded alongside BCO
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
      has changed.  The current code in GHC.Driver.Make handles this case
      fairly poorly, so be careful.

  See also Note [When source is considered modified]
-}

upsweep_mods
  :: GhcMonad m
  => Maybe Messager
  -> HomePackageTable
  -> LoadHowMuch
  -> ModuleGraph
  -> m (SuccessFlag, [ModuleGraphNode])
upsweep_mods mHscMessage old_hpt how_much mod_graph = do
  hsc_env <- getSession
  let dflags = hsc_dflags hsc_env
  let logger = hsc_logger hsc_env

  -- Build a list of topologically sorted nodes using the compilation graph.
  -- Module cycles are handled at the very end, after some useful work gets
  -- done.
  (nodes, cycle) <- liftIO $ buildUpsweepNodes how_much mod_graph

  liftIO $ debugTraceMsg logger 2 (hang (text "Ready for upsweep")
                                  2 (ppr nodes))

  -- The old HPT is used for recompilation checking in upsweep_mod. When a
  -- module successfully gets compiled, its HMI is pruned from the old HPT.
  old_hpt_var <- liftIO $ newIORef old_hpt

  n_jobs <- case parMakeCount dflags of
                  Nothing -> liftIO getNumProcessors
                  Just n  -> return n
  let upsweep_fn | n_jobs > 1 = parUpsweep n_jobs
                 | otherwise  = upsweep

  results <- upsweep_fn mHscMessage old_hpt_var nodes

  -- Collect and return the ModSummaries of all the successful compiles.
  let ok_results = map fst ok_results'
      ok_results' = reverse $ (catMaybes results)

  -- Update the global session with the new HomePackageTable.
  setSession $ mergeHPTs hsc_env (map snd ok_results')

  -- Handle any cycle in the original compilation graph and return the result
  -- of the upsweep.
  case cycle of
    Just mss -> do
      -- TODO: dont compute cycle / graph again in fatalErrorMsg
      liftIO $ fatalErrorMsg logger (cyclicModuleErr mss)
      return (Failed,ok_results)
    Nothing  -> do
      let success_flag = successIf (all isJust results)
      return (success_flag,ok_results)


{- Parallel Upsweep
 -
 - The parallel upsweep attempts to concurrently compile the modules in the
 - compilation graph using multiple Haskell threads.
 -
 - The Algorithm
 -
 - A Haskell thread is spawned for each UpsweepNode, waiting for its direct
 - dependencies to finish building before it itself begins to build.
 -
 - Each UpsweepNode is associated with an initially empty MVar that stores the
 - result of that particular node's compile. If the compile succeeded, then the
 - new HMI is put in the HPT, and the HPT is put in the MVar to unblock the
 - compilation of threads waiting on the current node. The module's HMI is
 - deleted from the old HPT (synchronized by an IORef) to save space.
 -
 - Instead of immediately outputting messages to the standard handles, all
 - compilation output is deferred to a per-module TQueue. A QSem is used to
 - limit the number of workers that are compiling simultaneously.
 -
 - Meanwhile, the main thread sequentially loops over all the modules in the
 - module graph, outputting the messages stored in each module's TQueue.
-}

-- | The entry point to the parallel upsweep.
--
-- See also the simpler, sequential 'upsweep'.
parUpsweep
    :: GhcMonad m
    => Int
    -- ^ The number of workers we wish to run in parallel
    -> Maybe Messager
    -> IORef HomePackageTable
    -> [UpsweepNode]
    -> m [Maybe (ModuleGraphNode, HomePackageTable)]
parUpsweep n_jobs mHscMessage old_hpt_var nodes = do
    hsc_env <- getSession
    let logger = hsc_logger hsc_env
    let tmpfs  = hsc_tmpfs hsc_env

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

    MC.bracket updNumCapabilities resetNumCapabilities $ \_ -> do

    liftIO $ label_self "main --make thread"

    -- Make the logger thread_safe: we only make the "log" action thread-safe in
    -- each worker by setting a LogAction hook, so we need to make the logger
    -- thread-safe for other actions (DumpAction, TraceAction).
    thread_safe_logger <- liftIO $ makeThreadSafe logger

    let
      num_nodes = length nodes
      -- Limit the number of parallel compiles.
      withSem :: IO a -> IO a
      withSem = MC.bracket_ (waitQSem par_sem) (signalQSem par_sem)

    -- For each module in the module graph, spawn a worker thread that will
    -- compile this module.
    let { spawnWorkers = forM (zip nodes [1..]) $ \(node,!mod_idx) ->
            forkIOWithUnmask $ \unmask -> do
              liftIO $ label_self $ unwords $ concat
                  [ [ "worker --make thread" ]
                  , case un_node node of
                      InstantiationNode iuid ->
                        [ "for instantiation of unit"
                        , show $ VirtUnit iuid
                        ]
                      ModuleNode ems ->
                        [ "for module"
                        , show (moduleNameString (ms_mod_name (emsModSummary ems)))
                        , show (isBootSummary (emsModSummary ems))
                        ]
                  , ["number"
                    , show mod_idx
                    ]
                  ]
              -- Replace the default logger with one that writes each
              -- message to the module's log_queue. The main thread will
              -- deal with synchronously printing these messages.
              let lcl_logger = pushLogHook (const (parLogAction $ un_logQueue node)) thread_safe_logger

              -- Use a local TmpFs so that we can clean up intermediate files
              -- in a timely fashion (as soon as compilation for that module
              -- is finished) without having to worry about accidentally
              -- deleting a simultaneous compile's important files.
              lcl_tmpfs <- forkTmpFsFrom tmpfs

              let lcl_hsc_env0 = hsc_env { hsc_logger = lcl_logger
                                        , hsc_tmpfs  = lcl_tmpfs
                                        }

              -- Wait for the all the dependencies to finish building.
              deps_res <- mapM readMVar (map snd $ un_depsMVars node)

              let lcl_hsc_env = mergeHPTs lcl_hsc_env0 (map snd deps_res)

              -- We can't build this module if any of its dependencies failed to build.
              m_res <- if not (all (succeeded . fst) deps_res)
                then return $ Right (Failed, hsc_HPT lcl_hsc_env)
                -- Unmask asynchronous exceptions and perform the thread-local
                -- work to compile the module (see upsweep_one).
                else MC.try $ unmask $ prettyPrintGhcErrors logger $ withSem $ do
                  upsweep_one node
                    mHscMessage
                    lcl_hsc_env old_hpt_var
                    mod_idx num_nodes

              res <- case m_res of
                  Right flag -> return flag
                  Left exc -> do
                      -- Don't print ThreadKilled exceptions: they are used
                      -- to kill the worker thread in the event of a user
                      -- interrupt, and the user doesn't have to be informed
                      -- about that.
                      when (fromException exc /= Just ThreadKilled)
                           (errorMsg lcl_logger (text (show exc)))
                      return (Failed, hsc_HPT lcl_hsc_env)

              -- Populate the result MVar.
              putMVar (un_resultMVar node) res

              -- Write the end marker to the message queue, telling the main
              -- thread that it can stop waiting for messages from this
              -- particular compile.
              writeLogQueue (un_logQueue node) Nothing

              -- Add the remaining files that weren't cleaned up to the
              -- global TmpFs, for cleanup later.
              mergeTmpFsInto lcl_tmpfs tmpfs

        -- Kill all the workers, masking interrupts (since killThread is
        -- interruptible). XXX: This is not ideal.
        ; killWorkers = MC.uninterruptibleMask_ . mapM_ killThread }


    -- Spawn the workers, making sure to kill them later. Collect the results
    -- of each compile.
    liftIO $ MC.bracket spawnWorkers killWorkers $ \_ ->
        -- Loop over each module in the compilation graph in order, printing
        -- each message from its log_queue.
        forM nodes $ \node -> do
            printLogs logger (un_logQueue node)
            (result, hpt) <- readMVar (un_resultMVar node)
            let mod = un_node node
            if succeeded result then return (Just (mod, hpt)) else return Nothing

  where
    writeLogQueue :: LogQueue -> Maybe (MessageClass,SrcSpan,SDoc) -> IO ()
    writeLogQueue (LogQueue ref sem) msg = do
        atomicModifyIORef' ref $ \msgs -> (msg:msgs,())
        _ <- tryPutMVar sem ()
        return ()

    -- The log_action callback that is used to synchronize messages from a
    -- worker thread.
    parLogAction :: LogQueue -> LogAction
    parLogAction log_queue _dflags !msgClass !srcSpan !msg =
        writeLogQueue log_queue (Just (msgClass,srcSpan,msg))

    -- Print each message from the log_queue using the global logger
    printLogs :: Logger -> LogQueue -> IO ()
    printLogs !logger (LogQueue ref sem) = read_msgs
      where read_msgs = do
                takeMVar sem
                msgs <- atomicModifyIORef' ref $ \xs -> ([], reverse xs)
                print_loop msgs

            print_loop [] = read_msgs
            print_loop (x:xs) = case x of
                Just (msgClass,srcSpan,msg) -> do
                    logMsg logger msgClass srcSpan msg
                    print_loop xs
                -- Exit the loop once we encounter the end marker.
                Nothing -> return ()

upsweep
    :: GhcMonad m
    => Maybe Messager
    -> IORef HomePackageTable
    -> [UpsweepNode]
    -> m [Maybe (ModuleGraphNode, HomePackageTable)]
upsweep mHscMessage old_hpt_var nodes = do
  hsc_env <- getSession
  let dflags = hsc_dflags hsc_env
      logger = hsc_logger hsc_env
      keep_going = gopt Opt_KeepGoing dflags
      num_nodes = length nodes

  let
    runComp _ [] = return []
    runComp !mod_idx (m:ms) = do
      (mod, (result, hpt)) <- runOne mod_idx m
      if succeeded result
        then ((Just (mod, hpt)) :) <$> runComp (mod_idx + 1) ms
        else (Nothing :) <$> (if keep_going then runComp mod_idx ms else return [])

    runOne !mod_idx node = do
      -- At this point all the MVars we depend upon should be filled in
      deps_res' <- mapM tryReadMVar (map snd $ un_depsMVars node)

      debugTraceMsg logger 4 $ vcat $
        [ (text "upsweep: runOne for" <+> ppr (mkNodeKey $ un_node node))]
        ++ (map (\(mres, (nk, _)) -> nest 2 (ppr nk <+> ppr mres))
            (zip (map (fmap fst) deps_res') (un_depsMVars node)))

      when (any isNothing deps_res') $ panic "upsweep: dependency's result MVar is empty"

      let lcl_hsc_env = mergeHPTs hsc_env (map snd deps_res)
          deps_res = mapMaybe id deps_res'

      -- We can't build this module if any of its dependencies failed to build.
      res <- if not (all (succeeded . fst) deps_res)
        then return $ (Failed, hsc_HPT lcl_hsc_env)
        else prettyPrintGhcErrors logger $ do
          upsweep_one node
            mHscMessage
            lcl_hsc_env old_hpt_var
            mod_idx num_nodes

      putMVar (un_resultMVar node) res
      return (un_node node, res)

  liftIO $ runComp 1 nodes

-- The interruptible subset of the worker threads' work.
upsweep_one
    :: UpsweepNode
    -- ^ The thing we wish to compile
    -> Maybe Messager
    -- ^ The messager
    -> HscEnv
    -- ^ The input HscEnv, with HomePackageTable containing all the deps
    -> IORef HomePackageTable
    -- ^ The old HPT
    -> Int
    -- ^ The index of this node
    -> Int
    -- ^ The total number of nodes
    -> IO (SuccessFlag, HomePackageTable)
    -- ^ The result of this compile

upsweep_one (UpsweepNode { un_node = InstantiationNode iuid})
  mHscMessage hsc_env0 _old_hpt_var mod_index num_mods
  = do
  upsweep_inst hsc_env0 mHscMessage mod_index num_mods iuid
  pure (Succeeded, hsc_HPT hsc_env0)

upsweep_one (UpsweepNode { un_node = ModuleNode ems, un_mLoopCloser = finish_loop})
  mHscMessage hsc_env0 old_hpt_var mod_index num_mods
  = do
  old_hpt <- readIORef old_hpt_var

  handleSourceError (\err -> do logg err; return (Failed, hsc_HPT hsc_env1)) $ do
    mod_info <- do
      -- Re-typecheck the loop
      -- This is necessary to make sure the knot is tied when
      -- we close a recursive module loop, see bug #12035.
      type_env_var <- liftIO $ newIORef emptyNameEnv
      let hsc_env2 = hsc_env1 { hsc_type_env_var =
                          Just (ms_mod mod, type_env_var) }
      hsc_env3 <- case finish_loop of
          Nothing   -> return hsc_env2
          -- In the non-parallel case, the retypecheck prior to
          -- typechecking the loop closer includes all modules
          -- EXCEPT the loop closer.  However, our precomputed
          -- SCCs include the loop closer, so we have to filter
          -- it out.
          Just loop -> typecheckLoop hsc_env2 $
            [ mod
              | NodeKey_Module (GWIB { gwib_mod = mod, gwib_isBoot = NotBoot }) <- NonEmpty.tail loop
              , mod /= this_mod ]

      -- Compile the module.
      upsweep_mod hsc_env3 mHscMessage old_hpt
                              mod mod_index num_mods


    -- Prune the old HPT unless this is an hs-boot module.
    unless (isBootSummary mod == IsBoot) $
        atomicModifyIORef' old_hpt_var $ \old_hpt ->
            (delFromHpt old_hpt this_mod, ())

    hsc_env5 <- do
        let hsc_env4 = hscUpdateHPT (\hpt -> addToHpt hpt this_mod mod_info)
                                    hsc_env1

        -- We've finished typechecking the module, now we must
        -- retypecheck the loop AGAIN to ensure unfoldings are
        -- updated.  This time, however, we include the loop
        -- closer!
        case finish_loop of
            Nothing   -> return hsc_env4
            Just loop -> typecheckLoop hsc_env4 $
              [ mod | NodeKey_Module (GWIB { gwib_mod = mod, gwib_isBoot = NotBoot }) <- NonEmpty.tail loop]

    -- Add any necessary entries to the static pointer
    -- table. See Note [Grand plan for static forms] in
    -- GHC.Iface.Tidy.StaticPtrTable.
    when (backend (hsc_dflags hsc_env5) == Interpreter) $
        liftIO $ hscAddSptEntries hsc_env5 (Just (ms_mnwib mod))
                     [ spt
                     | Just linkable <- pure $ hm_linkable mod_info
                     , unlinked <- linkableUnlinked linkable
                     , BCOs _ spts <- pure unlinked
                     , spt <- spts
                     ]

    -- Clean up any intermediate files.
    cleanCurrentModuleTempFiles (hsc_logger hsc_env5)
                                (hsc_tmpfs  hsc_env5)
                                (hsc_dflags hsc_env5)
    return (Succeeded, hsc_HPT hsc_env5)

  where
    hsc_env1 = hsc_env0 { hsc_logger = lcl_logger }

    lcl_diag_opts = initDiagOpts lcl_dflags
    logg err = printMessages lcl_logger lcl_diag_opts (srcErrorMessages err)

    this_mod = ms_mod_name mod

    mod = emsModSummary ems
    lcl_dflags  = ms_hspp_opts mod
    lcl_logger = setLogFlags (hsc_logger hsc_env0) (initLogFlags lcl_dflags)

-- | Update/merge the HPT, while avoiding the overwrite of HomeModeInfo of
-- HsSrcFile by the HsBootFile
mergeHPTs :: HscEnv -> [HomePackageTable] -> HscEnv
mergeHPTs = foldr (\h -> hscUpdateHPT (\hpt -> foldUDFM mergeOne hpt h))
  where
  mergeOne newV oldHpt = case lookupHpt oldHpt modName of
    Nothing -> addNewV
    Just _ -> case (mi_hsc_src $ hm_iface newV) of
      HsBootFile -> oldHpt
      _ -> addNewV
    where modName = moduleName $ mi_module $ hm_iface $ newV
          addNewV = addToHpt oldHpt modName newV

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
            -> HomePackageTable
            -> ModSummary
            -> Int  -- index of module
            -> Int  -- total number of modules
            -> IO HomeModInfo
upsweep_mod hsc_env mHscMessage old_hpt summary mod_index nmods
   =    let
            old_hmi = lookupHpt old_hpt (ms_mod_name summary)

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
                     Nothing                                        -> Nothing
                     Just hm_info | isBootSummary summary == IsBoot -> Just iface
                                  | mi_boot iface == NotBoot        -> Just iface
                                  | otherwise                       -> Nothing
                                   where
                                     iface = hm_iface hm_info

            compile_it :: Maybe Linkable -> IO HomeModInfo
            compile_it  mb_linkable =
                  compileOne' mHscMessage hsc_env summary mod_index nmods
                             mb_old_iface mb_linkable

        in
          compile_it (old_hmi >>= hm_linkable)


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

Following this fix, GHC can compile itself with --make -O2.
-}

-- NB: sometimes mods has duplicates; this is harmless because
-- any duplicates get clobbered in addListToHpt and never get forced.
typecheckLoop :: HscEnv -> [ModuleName] -> IO HscEnv
typecheckLoop hsc_env mods = do
  debugTraceMsg logger 2 $
     text "Re-typechecking loop: " <> ppr mods
  new_hpt <-
    fixIO $ \new_hpt -> do
      let new_hsc_env = hscUpdateHPT (const new_hpt) hsc_env
      mds <- initIfaceCheck (text "typecheckLoop") new_hsc_env $
                mapM (typecheckIface . hm_iface) hmis
      let new_hpt = addListToHpt old_hpt
                        (zip mods [ hmi{ hm_details = details }
                                  | (hmi,details) <- zip hmis mds ])
      return new_hpt
  return (hscUpdateHPT (const new_hpt) hsc_env)
  where
    logger  = hsc_logger hsc_env
    old_hpt = hsc_HPT hsc_env
    hmis    = map (expectJust "typecheckLoop" . lookupHpt old_hpt) mods

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



-- | ModNodeKey and ModNodeMap are (now) used exclusively for downsweep
-- (upsweep makes use of NodeKey to handle instantiation nodes)
type ModNodeKey = ModuleNameWithIsBoot
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

msKey :: ModSummary -> ModNodeKey
msKey = mkHomeBuildModule0

mkNodeMap :: [ExtendedModSummary] -> ModNodeMap ExtendedModSummary
mkNodeMap summaries = ModNodeMap $ Map.fromList
  [ (msKey $ emsModSummary s, s) | s <- summaries]

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
              tn <- newTempName logger tmpfs dflags staticLife suf
              let dyn_tn = tn -<.> dynsuf
              addFilesToClean tmpfs dynLife [dyn_tn]
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
            else (,) <$> (new_temp_file (hiSuf_ dflags) (dynHiSuf_ dflags))
                     <*> (new_temp_file (objectSuf_ dflags) (dynObjectSuf_ dflags))
        let ms' = ms
              { ms_location =
                  ms_location {ml_hi_file = hi_file, ml_obj_file = o_file}
              , ms_hspp_opts = updOptLevel 0 $
                  setOutputFile (Just o_file) $
                  setDynOutputFile (Just $ dynamicOutputFile dflags o_file) $
                  setOutputHi (Just hi_file) $
                  dflags {backend = bcknd}
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


        -- Make a ModLocation for this file
        location <- liftIO $ mkHomeModLocation (hsc_dflags hsc_env) pi_mod_name src_fn

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
    dflags    = hsc_dflags hsc_env
    home_unit = hsc_home_unit hsc_env
    fc        = hsc_FC hsc_env
    units     = hsc_units hsc_env

    check_hash old_summary location src_fn =
        checkSummaryHash
          hsc_env
          (new_summary location (ms_mod $ emsModSummary old_summary) src_fn)
          old_summary location

    find_it = do
        found <- findImportedModule fc units home_unit dflags wanted_mod Nothing
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

        when (hsc_src == HsigFile && isNothing (lookup pi_mod_name (homeUnitInstantiations home_unit))) $
            let instantiations = homeUnitInstantiations home_unit
            in throwE $ singleMessage $ mkPlainErrorMsgEnvelope pi_mod_name_loc
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
  let dflags = hsc_dflags hsc_env
  obj_timestamp <- modificationTimeIfExists (ml_obj_file nms_location)
  dyn_obj_timestamp <- modificationTimeIfExists (dynamicOutputFile dflags (ml_obj_file nms_location))
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
        , ms_textual_imps =
            pi_theimps ++
            extra_sig_imports ++
            ((,) Nothing . noLoc <$> implicit_sigs)
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
    -> ExceptT DriverMessages IO PreprocessedImports
getPreprocessedImports hsc_env src_fn mb_phase maybe_buf = do
  (pi_local_dflags, pi_hspp_fn)
      <- ExceptT $ preprocess hsc_env src_fn (fst <$> maybe_buf) mb_phase
  pi_hspp_buf <- liftIO $ hGetStringBuffer pi_hspp_fn
  (pi_srcimps, pi_theimps, L pi_mod_name_loc pi_mod_name)
      <- ExceptT $ do
          let imp_prelude = xopt LangExt.ImplicitPrelude pi_local_dflags
              popts = initParserOpts pi_local_dflags
          mimps <- getImports popts imp_prelude pi_hspp_buf pi_hspp_fn src_fn
          return (first (mkMessages . fmap mkDriverPsHeaderMessage . getMessages) mimps)
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
        [ NodeKey_Module $ GWIB { gwib_mod = unLoc m, gwib_isBoot = NotBoot }
        | m <- ms_home_imps    ms ] ++
        [ NodeKey_Unit inst_unit
        | inst_unit <- bds
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
    ppr_node (ModuleNode m) = text "module" <+> ppr_ms (emsModSummary m)
    ppr_node (InstantiationNode u) = text "instantiated unit" <+> ppr u

    ppr_ms :: ModSummary -> SDoc
    ppr_ms ms = quotes (ppr (moduleName (ms_mod ms))) <+>
                (parens (text (msHsFilePath ms)))

label_self :: String -> IO ()
label_self thread_name = do
    self_tid <- CC.myThreadId
    CC.labelThread self_tid thread_name

partitionNodes
  :: [ModuleGraphNode]
  -> ( [InstantiatedUnit]
     , [ExtendedModSummary]
     )
partitionNodes ns = partitionEithers $ flip fmap ns $ \case
  InstantiationNode x -> Left x
  ModuleNode x -> Right x
