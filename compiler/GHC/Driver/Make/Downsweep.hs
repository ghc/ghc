{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module GHC.Driver.Make.Downsweep where

import GHC.Prelude

import GHC.Tc.Utils.Backpack


import GHC.Platform.Ways

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

import GHC.Parser.Header


import GHC.Data.Graph.Directed
import GHC.Data.FastString
import GHC.Data.Maybe      ( expectJust )
import GHC.Data.StringBuffer
import qualified GHC.LanguageExtensions as LangExt

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Error
import GHC.Utils.Logger
import GHC.Utils.Fingerprint
import GHC.Utils.TmpFs

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
import GHC.Unit.Module.Graph

import Data.Either ( rights, partitionEithers, lefts )
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad
import Control.Monad.Trans.Except ( ExceptT(..), runExceptT, throwE )
import Data.Maybe
import Data.Time
import Data.Bifunctor (first)
import System.Directory
import System.FilePath

import qualified Data.Map.Strict as M
import GHC.Rename.Names
import GHC.Utils.Constants

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

-- Note [Unused packages]
-- ~~~~~~~~~~~~~~~~~~~~~~
-- Cabal passes `--package-id` flag for each direct dependency. But GHC
-- loads them lazily, so when compilation is done, we have a list of all
-- actually loaded packages. All the packages, specified on command line,
-- but never loaded, are probably unused dependencies.

warnUnusedPackages :: UnitState -> DynFlags -> ModuleGraph -> DriverMessages
warnUnusedPackages us dflags mod_graph =
    let diag_opts = initDiagOpts dflags

    -- Only need non-source imports here because SOURCE imports are always HPT
        loadedPackages = concat $
          mapMaybe (\(fs, mn) -> lookupModulePackage us (unLoc mn) fs)
            $ concatMap ms_imps (
              filter (\ms -> homeUnitId_ dflags == ms_unitid ms) (mgModSummaries mod_graph))

        used_args = Set.fromList $ map unitId loadedPackages

        resolve (u,mflag) = do
                  -- The units which we depend on via the command line explicitly
                  flag <- mflag
                  -- Which we can find the UnitInfo for (should be all of them)
                  ui <- lookupUnit us u
                  -- Which are not explicitly used
                  guard (Set.notMember (unitId ui) used_args)
                  return (unitId ui, unitPackageName ui, unitPackageVersion ui, flag)

        unusedArgs = mapMaybe resolve (explicitUnits us)

        warn = singleMessage $ mkPlainMsgEnvelope diag_opts noSrcSpan (DriverUnusedPackages unusedArgs)

    in if null unusedArgs
        then emptyMessages
        else warn

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
    liftIO $ flushFinderCaches (hsc_FC hsc_env) (hsc_unit_env hsc_env)

    (errs, graph_nodes) <- liftIO $ downsweep
      hsc_env (mgModSummaries old_graph)
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
warnMissingHomeModules ::  DynFlags -> [Target] -> ModuleGraph -> DriverMessages
warnMissingHomeModules dflags targets mod_graph =
    if null missing
      then emptyMessages
      else warn
  where
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
    is_my_target mod target =
      let tuid = targetUnitId target
      in case targetId target of
          TargetModule name
            -> moduleName (ms_mod mod) == name
                && tuid == ms_unitid mod
          TargetFile target_file _
            | Just mod_file <- ml_hs_file (ms_location mod)
            ->
             target_file == mod_file ||

             --  Don't warn on B.hs-boot if B.hs is specified (#16551)
             addBootSuffix target_file == mod_file ||

             --  We can get a file target even if a module name was
             --  originally specified in a command line because it can
             --  be converted in guessTarget (by appending .hs/.lhs).
             --  So let's convert it back and compare with module name
             mkModuleName (fst $ splitExtension target_file)
              == moduleName (ms_mod mod)
          _ -> False

    missing = map (moduleName . ms_mod) $
      filter (not . is_known_module) $
        (filter (\ms -> ms_unitid ms == homeUnitId_ dflags)
                (mgModSummaries mod_graph))

    warn = singleMessage $ mkPlainMsgEnvelope diag_opts noSrcSpan
                         $ DriverMissingHomeModules missing (checkBuildingCabalPackage dflags)

-- Check that any modules we want to reexport or hide are actually in the package.
warnUnknownModules :: HscEnv -> DynFlags -> ModuleGraph -> IO DriverMessages
warnUnknownModules hsc_env dflags mod_graph = do
  reexported_warns <- filterM check_reexport (Set.toList reexported_mods)
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
      fr <- lookupModule mn
      case fr of
        Found _ m -> return (moduleUnitId m == homeUnitId_ dflags)
        _ -> return True


    warn flag mod = singleMessage $ mkPlainMsgEnvelope diag_opts noSrcSpan
                         $ flag mod

    final_msgs hidden_warns reexported_warns
          =
        unionManyMessages $
          [warn DriverUnknownHiddenModules (Set.toList hidden_warns) | not (Set.null hidden_warns)]
          ++ [warn DriverUnknownReexportedModules reexported_warns | not (null reexported_warns)]

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
downsweep hsc_env old_summaries excl_mods allow_dup_roots
   = do
       rootSummaries <- mapM getRootSummary roots
       let (root_errs, rootSummariesOk) = partitionEithers rootSummaries -- #17549
           root_map = mkRootMap rootSummariesOk
       checkDuplicates root_map
       (deps, pkg_deps, map0) <- loopSummaries rootSummariesOk (M.empty, Set.empty, root_map)
       let closure_errs = checkHomeUnitsClosed (hsc_unit_env hsc_env) (hsc_all_home_unit_ids hsc_env) (Set.toList pkg_deps)
       let unit_env = hsc_unit_env hsc_env
       let tmpfs    = hsc_tmpfs    hsc_env

       let downsweep_errs = lefts $ concat $ M.elems map0
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
        roots  = hsc_targets hsc_env

        -- A cache from file paths to the already summarised modules.
        -- Reuse these if we can because the most expensive part of downsweep is
        -- reading the headers.
        old_summary_map :: M.Map FilePath ModSummary
        old_summary_map = M.fromList [(msHsFilePath ms, ms) | ms <- old_summaries]

        getRootSummary :: Target -> IO (Either (UnitId, DriverMessages) ModSummary)
        getRootSummary Target { targetId = TargetFile file mb_phase
                              , targetContents = maybe_buf
                              , targetUnitId = uid
                              }
           = do let offset_file = augmentByWorkingDirectory dflags file
                exists <- liftIO $ doesFileExist offset_file
                if exists || isJust maybe_buf
                    then first (uid,) <$>
                        summariseFile hsc_env home_unit old_summary_map offset_file mb_phase
                                       maybe_buf
                    else return $ Left $ (uid,) $ singleMessage
                                $ mkPlainErrorMsgEnvelope noSrcSpan (DriverFileNotFound offset_file)
            where
              dflags = homeUnitEnv_dflags (ue_findHomeUnitEnv uid (hsc_unit_env hsc_env))
              home_unit = ue_unitHomeUnit uid (hsc_unit_env hsc_env)
        getRootSummary Target { targetId = TargetModule modl
                              , targetContents = maybe_buf
                              , targetUnitId = uid
                              }
           = do maybe_summary <- summariseModule hsc_env home_unit old_summary_map NotBoot
                                           (L rootLoc modl) (ThisPkg (homeUnitId home_unit))
                                           maybe_buf excl_mods
                case maybe_summary of
                   FoundHome s  -> return (Right s)
                   FoundHomeWithError err -> return (Left err)
                   _ -> return $ Left $ (uid, moduleNotFoundErr modl)
            where
              home_unit = ue_unitHomeUnit uid (hsc_unit_env hsc_env)
        rootLoc = mkGeneralSrcSpan (fsLit "<command line>")

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
              -> (M.Map NodeKey ModuleGraphNode, Set.Set (UnitId, UnitId),
                    DownsweepCache)
              -> IO ((M.Map NodeKey ModuleGraphNode), Set.Set (UnitId, UnitId), DownsweepCache)
        loopSummaries [] done = return done
        loopSummaries (ms:next) (done, pkgs, summarised)
          | Just {} <- M.lookup k done
          = loopSummaries next (done, pkgs, summarised)
          -- Didn't work out what the imports mean yet, now do that.
          | otherwise = do
             (final_deps, pkgs1, done', summarised') <- loopImports (calcDeps ms) done summarised
             -- This has the effect of finding a .hs file if we are looking at the .hs-boot file.
             (_, _, done'', summarised'') <- loopImports (maybeToList hs_file_for_boot) done' summarised'
             loopSummaries next (M.insert k (ModuleNode final_deps ms) done'', pkgs1 `Set.union` pkgs, summarised'')
          where
            k = NodeKey_Module (msKey ms)

            hs_file_for_boot
              | HsBootFile <- ms_hsc_src ms = Just $ ((ms_unitid ms), NoPkgQual, (GWIB (noLoc $ ms_mod_name ms) NotBoot))
              | otherwise = Nothing


        -- This loops over each import in each summary. It is mutually recursive with loopSummaries if we discover
        -- a new module by doing this.
        loopImports :: [(UnitId, PkgQual, GenWithIsBoot (Located ModuleName))]
                        -- Work list: process these modules
             -> M.Map NodeKey ModuleGraphNode
             -> DownsweepCache
                        -- Visited set; the range is a list because
                        -- the roots can have the same module names
                        -- if allow_dup_roots is True
             -> IO ([NodeKey], Set.Set (UnitId, UnitId),

                  M.Map NodeKey ModuleGraphNode, DownsweepCache)
                        -- The result is the completed NodeMap
        loopImports [] done summarised = return ([], Set.empty, done, summarised)
        loopImports ((home_uid,mb_pkg, gwib) : ss) done summarised
          | Just summs <- M.lookup cache_key summarised
          = case summs of
              [Right ms] -> do
                let nk = NodeKey_Module (msKey ms)
                (rest, pkgs, summarised', done') <- loopImports ss done summarised
                return (nk: rest, pkgs, summarised', done')
              [Left _err] ->
                loopImports ss done summarised
              _errs ->  do
                loopImports ss done summarised
          | otherwise
          = do
               mb_s <- summariseModule hsc_env home_unit old_summary_map
                                       is_boot wanted_mod mb_pkg
                                       Nothing excl_mods
               case mb_s of
                   NotThere -> loopImports ss done summarised
                   External uid -> do
                    (other_deps, pkgs, done', summarised') <- loopImports ss done summarised
                    return (other_deps, Set.insert (homeUnitId home_unit, uid) pkgs, done', summarised')
                   FoundInstantiation iud -> do
                    (other_deps, pkgs, done', summarised') <- loopImports ss done summarised
                    return (NodeKey_Unit iud : other_deps, pkgs, done', summarised')
                   FoundHomeWithError (_uid, e) ->  loopImports ss done (Map.insert cache_key [(Left e)] summarised)
                   FoundHome s -> do
                     (done', pkgs1, summarised') <-
                       loopSummaries [s] (done, Set.empty, Map.insert cache_key [Right s] summarised)
                     (other_deps, pkgs2, final_done, final_summarised) <- loopImports ss done' summarised'

                     -- MP: This assumes that we can only instantiate non home units, which is probably fair enough for now.
                     return (NodeKey_Module (msKey s) : other_deps, pkgs1 `Set.union` pkgs2, final_done, final_summarised)
          where
            cache_key = (home_uid, mb_pkg, unLoc <$> gwib)
            home_unit = ue_unitHomeUnit home_uid (hsc_unit_env hsc_env)
            GWIB { gwib_mod = L loc mod, gwib_isBoot = is_boot } = gwib
            wanted_mod = L loc mod

-- This function checks then important property that if both p and q are home units
-- then any dependency of p, which transitively depends on q is also a home unit.
checkHomeUnitsClosed ::  UnitEnv -> Set.Set UnitId -> [(UnitId, UnitId)] -> [DriverMessages]
-- Fast path, trivially closed.
checkHomeUnitsClosed ue home_id_set home_imp_ids
  | Set.size home_id_set == 1 = []
  | otherwise =
  let res = foldMap loop home_imp_ids
  -- Now check whether everything which transitively depends on a home_unit is actually a home_unit
  -- These units are the ones which we need to load as home packages but failed to do for some reason,
  -- it's a bug in the tool invoking GHC.
      bad_unit_ids = Set.difference res home_id_set
  in if Set.null bad_unit_ids
        then []
        else [singleMessage $ mkPlainErrorMsgEnvelope rootLoc $ DriverHomePackagesNotClosed (Set.toList bad_unit_ids)]

  where
    rootLoc = mkGeneralSrcSpan (fsLit "<command line>")
    -- TODO: This could repeat quite a bit of work but I struggled to write this function.
    -- Which units transitively depend on a home unit
    loop :: (UnitId, UnitId) -> Set.Set UnitId -- The units which transitively depend on a home unit
    loop (from_uid, uid) =
      let us = ue_findHomeUnitEnv from_uid ue in
      let um = unitInfoMap (homeUnitEnv_units us) in
      case Map.lookup uid um of
        Nothing -> pprPanic "uid not found" (ppr uid)
        Just ui ->
          let depends = unitDepends ui
              home_depends = Set.fromList depends `Set.intersection` home_id_set
              other_depends = Set.fromList depends `Set.difference` home_id_set
          in
            -- Case 1: The unit directly depends on a home_id
            if not (null home_depends)
              then
                let res = foldMap (loop . (from_uid,)) other_depends
                in Set.insert uid res
             -- Case 2: Check the rest of the dependencies, and then see if any of them depended on
              else
                let res = foldMap (loop . (from_uid,)) other_depends
                in
                  if not (Set.null res)
                    then Set.insert uid res
                    else res

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
               let new_dflags = case enable_spec of
                                  EnableByteCode -> dflags { backend = interpreterBackend }
                                  EnableObject   -> dflags { backend = defaultBackendOf ms }
                                  EnableByteCodeAndObject -> (gopt_set dflags Opt_ByteCodeAndObjectCode) { backend = defaultBackendOf ms}
               let ms' = ms
                     { ms_location =
                         ms_location { ml_hi_file = hi_file
                                     , ml_obj_file = o_file
                                     , ml_dyn_hi_file = dyn_hi_file
                                     , ml_dyn_obj_file = dyn_o_file }
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
      = hostIsDynamic && internalInterpreter &&
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
        -> M.Map FilePath ModSummary    -- old summaries
        -> FilePath                     -- source file name
        -> Maybe Phase                  -- start phase
        -> Maybe (StringBuffer,UTCTime)
        -> IO (Either DriverMessages ModSummary)

summariseFile hsc_env' home_unit old_summaries src_fn mb_phase maybe_buf
        -- we can use a cached summary if one is available and the
        -- source file hasn't changed,  But we have to look up the summary
        -- by source file, rather than module name as we do in summarise.
   | Just old_summary <- M.lookup src_fn old_summaries
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
           -- Also, only add to finder cache for non-boot modules as the finder cache
           -- makes sure to add a boot suffix for boot files.
           _ <- do
              let fc        = hsc_FC hsc_env
              case ms_hsc_src old_summary of
                HsSrcFile -> addModuleToFinder fc (ms_mod old_summary) location
                _ -> return ()

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
          -> M.Map FilePath ModSummary
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
        found <- findImportedModule hsc_env wanted_mod mb_pkg
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
        let location' = case is_boot of
              IsBoot -> addBootSuffixLocn location
              NotBoot -> location
            src_fn = expectJust "summarise2" (ml_hs_file location')

                -- Check that it exists
                -- It might have been deleted since the Finder last found it
        maybe_h <- fileHashIfExists src_fn
        case maybe_h of
          -- This situation can also happen if we have found the .hs file but the
          -- .hs-boot file doesn't exist.
          Nothing -> return NotThere
          Just h  -> do
            fresult <- new_summary_cache_check location' mod src_fn h
            return $ case fresult of
              Left err -> FoundHomeWithError (moduleUnitId mod, err)
              Right ms -> FoundHome ms

    new_summary_cache_check loc mod src_fn h
      | Just old_summary <- Map.lookup src_fn old_summary_map =

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
              | is_boot == IsBoot = HsBootFile
              | isHaskellSigFilename src_fn = HsigFile
              | otherwise = HsSrcFile

        when (pi_mod_name /= wanted_mod) $
                throwE $ singleMessage $ mkPlainErrorMsgEnvelope pi_mod_name_loc
                       $ DriverFileModuleNameMismatch pi_mod_name wanted_mod

        let instantiations = homeUnitInstantiations home_unit
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