-- The transition from Int to Word64 for uniques makes functions slightly larger
-- without this GHC option some optimizations fail to fire.
-- See https://gitlab.haskell.org/ghc/ghc/-/merge_requests/10568#note_505751
{-# OPTIONS_GHC -fspec-constr-threshold=10000 #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module GHC.Linker.Deps
  ( LinkDepsOpts (..)
  , LinkDeps (..)
  , getLinkDeps
  )
where

import GHC.Prelude

import GHC.Platform.Ways

import GHC.Runtime.Interpreter

import GHC.Linker.Types

import GHC.Types.SrcLoc
import GHC.Types.Unique.DSet
import GHC.Types.Unique.DFM

import GHC.Utils.Outputable
import qualified GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import GHC.Utils.Error

import GHC.Unit.Env
import GHC.Unit.Finder
import GHC.Unit.Module
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.WholeCoreBindings
import GHC.Unit.Module.Deps
import GHC.Unit.Module.Graph
import GHC.Unit.Home.ModInfo

import GHC.Iface.Errors.Types
import GHC.Iface.Errors.Ppr

import GHC.Utils.Misc
import GHC.Unit.Home
import GHC.Data.Maybe

import Control.Applicative
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)

import Data.Foldable (traverse_)
import qualified Data.Set as Set
import qualified Data.Map as M
import Data.List (isSuffixOf)

import System.FilePath
import System.Directory
import GHC.Utils.Logger (Logger)
import Control.Monad ((<$!>))

data LinkDepsOpts = LinkDepsOpts
  { ldObjSuffix   :: !String                        -- ^ Suffix of .o files
  , ldOneShotMode :: !Bool                          -- ^ Is the driver in one-shot mode?
  , ldModuleGraph :: !ModuleGraph
  , ldUnitEnv     :: !UnitEnv
  , ldPprOpts     :: !SDocContext                   -- ^ Rendering options for error messages
  , ldUseByteCode :: !Bool                          -- ^ Use bytecode rather than objects
  , ldPkgByteCode :: !Bool                          -- ^ Use bytecode for external packages
  , ldMsgOpts     :: !(DiagnosticOpts IfaceMessage) -- ^ Options for diagnostics
  , ldWays        :: !Ways                          -- ^ Enabled ways
  , ldLoadIface :: !(SDoc -> Module -> IO (MaybeErr MissingInterfaceError (ModIface, ModLocation)))
  -- ^ When linking oneshot or package dependencies, we need interfaces and
  -- locations to find object files and traverse dependencies.
  , ldLoadByteCode :: !(Module -> IO (Maybe (IO Linkable)))
  -- ^ Consult the EPS about the given module, return an action that compiles
  -- Core bindings to bytecode if it's available.
  , ldLogger :: !Logger
  }

data LinkDeps = LinkDeps
  { ldNeededLinkables :: [Linkable]
  , ldAllLinkables    :: [Linkable]
  , ldNeededUnits     :: [UnitId]
  , ldAllUnits        :: UniqDSet UnitId
  }

-- | Find all the packages and linkables that a set of modules depends on
--
-- Return the module and package dependencies for the needed modules.
-- See Note [Object File Dependencies]
--
-- Fails with an IO exception if it can't find enough files
--
getLinkDeps
  :: LinkDepsOpts
  -> Interp
  -> LoaderState
  -> SrcSpan      -- for error messages
  -> [Module]     -- If you need these
  -> IO LinkDeps  -- ... then link these first
getLinkDeps opts interp pls span mods = do
      -- The interpreter and dynamic linker can only handle object code built
      -- the "normal" way, i.e. no non-std ways like profiling or ticky-ticky.
      -- So here we check the build tag: if we're building a non-standard way
      -- then we need to find & link object files built the "normal" way.
      maybe_normal_osuf <- checkNonStdWay opts interp span

      withTiming (ldLogger opts) (text "getLinkDeps" <+> brackets (ppr span)) (const ()) $
        get_link_deps opts pls maybe_normal_osuf span mods

-- | Determine which parts of a module and its dependencies should be linked
-- when resolving external dependencies.
data LinkExternalDetails =
  -- | A module that should be linked, including its dependencies in the home
  -- unit and external packages.
  -- Can be a home module in oneshot mode or a package dependency module in
  -- either mode.
  LinkAllDeps
  |
  -- | A home module whose package dependencies should be linked, but not the
  -- module itself or its home unit dependencies.
  -- Can either be a direct target or the implementation module corresponding to
  -- a target boot module, but only in make mode.
  -- The 'ModIface' is taken from the 'HomeModInfo', avoiding another lookup in
  -- 'external_deps'.
  -- The importing module and its home unit dependencies are not processed by
  -- 'external_deps', since the readily available 'HomeModInfo's can be linked
  -- without further analysis.
  LinkOnlyPackages !ModIface

instance Outputable LinkExternalDetails where
  ppr = \case
    LinkAllDeps -> text "all"
    LinkOnlyPackages _ -> text "only-packages"

-- | A module that should be examined by 'external_deps' to decide how to link
-- it and its dependencies.
data LinkExternal =
  LinkExternal {
    le_details :: LinkExternalDetails,
    le_module :: !Module
  }

instance Outputable LinkExternal where
  ppr LinkExternal {..} = ppr le_module <> brackets (ppr le_details)

-- | The decision about the linking method used for a given module.
data LinkModule =
  -- | In make mode, we can use 'HomeModInfo' without any further analysis.
  LinkHomeModule !HomeModInfo
  |
  -- | A module that must be linked as native code, because bytecode is disabled
  -- or unavailable.
  LinkObjectModule !Module !ModLocation
  |
  -- | A module that has bytecode available.
  -- The 'IO' that compiles the bytecode from Core bindings is obtained from the
  -- EPS.
  -- See Note [Interface Files with Core Definitions].
  LinkByteCodeModule !Module !(IO Linkable)

link_module :: LinkModule -> Module
link_module = \case
  LinkHomeModule hmi -> mi_module (hm_iface hmi)
  LinkObjectModule mod _ -> mod
  LinkByteCodeModule mod _ -> mod

instance Outputable LinkModule where
  ppr = \case
    LinkHomeModule hmi -> ppr (mi_module (hm_iface hmi)) <+> brackets (text "HMI")
    LinkObjectModule mod _ -> ppr mod
    LinkByteCodeModule mod _ -> ppr mod <+> brackets (text "BC")

-- | Compute the linkables for the given module set's dependencies.
--
-- Home modules in make mode are treated separately in a preprocessing step,
-- then all the remaining external deps are processed for both modes.
-- If bytecode is available, transitive external deps are included, otherwise
-- the module's library is linked and processing stops.
--
-- The results are split into sets of needed/loaded modules/packages.
get_link_deps
  :: LinkDepsOpts
  -> LoaderState
  -> Maybe FilePath  -- replace object suffixes?
  -> SrcSpan
  -> [Module]
  -> IO LinkDeps
get_link_deps opts pls maybe_normal_osuf span mods = do
  (link_deps_home, module_deps_external) <- separate_home_deps
  link_deps_external <- external_deps opts module_deps_external
  let (loaded_modules, needed_modules, ldAllUnits, ldNeededUnits) =
        classify_deps pls link_deps_home link_deps_external
  ldNeededLinkables <- mapM module_linkable needed_modules
  pure LinkDeps {
    ldNeededLinkables,
    ldAllLinkables = loaded_modules ++ ldNeededLinkables,
    ldNeededUnits,
    ldAllUnits
  }
  where
    mod_graph = ldModuleGraph opts
    unit_env  = ldUnitEnv     opts
    noninteractive = filterOut isInteractiveModule mods

    -- Preprocess the dependencies in make mode to remove all home modules,
    -- since the transitive dependency closure is already cached for those in
    -- the HUG (see MultiLayerModulesTH_* tests for the performance impact).
    --
    -- Returns the remaining, external, dependencies on the right, which is the
    -- entire set for oneshot mode.
    separate_home_deps =
      if ldOneShotMode opts
      then pure ([], LinkExternal LinkAllDeps <$!> noninteractive)
      else make_deps

    make_deps = do
      (dep_ext, mmods) <- unzip <$> mapM get_mod_info all_home_mods
      pure (mmods, init_ext ++ dep_ext)

    -- This code is used in `--make` mode to calculate the home package and unit dependencies
    -- for a set of modules.
    --
    -- It is significantly more efficient to use the shared transitive dependency
    -- calculation than to compute the transitive dependency set in the same manner as oneShot mode.

    -- It is also a matter of correctness to use the module graph so that dependencies between home units
    -- is resolved correctly.
    make_deps_loop :: ([LinkExternal], Set.Set NodeKey) -> [ModNodeKeyWithUid] -> ([LinkExternal], Set.Set NodeKey)
    make_deps_loop found [] = found
    make_deps_loop found@(external, found_mods) (nk:nexts)
      | NodeKey_Module nk `Set.member` found_mods = make_deps_loop found nexts
      | otherwise =
        case M.lookup (NodeKey_Module nk) (mgTransDeps mod_graph) of
            Just trans_deps ->
              let deps = Set.insert (NodeKey_Module nk) trans_deps
                  -- See #936 and the ghci.prog007 test for why we have to continue traversing through
                  -- boot modules.
                  todo_boot_mods = [ModNodeKeyWithUid (GWIB mn NotBoot) uid | NodeKey_Module (ModNodeKeyWithUid (GWIB mn IsBoot) uid) <- Set.toList trans_deps]
              in make_deps_loop (external, deps `Set.union` found_mods) (todo_boot_mods ++ nexts)
            Nothing ->
              let (ModNodeKeyWithUid (GWIB mod_name _) uid) = nk
                  mod = Module (RealUnit (Definite uid)) mod_name
              in make_deps_loop (LinkExternal LinkAllDeps mod : external, found_mods) nexts

    mkNk m = ModNodeKeyWithUid (GWIB (moduleName m) NotBoot) (moduleUnitId m)
    (init_ext, all_deps) = make_deps_loop ([], Set.empty) $ map mkNk noninteractive

    all_home_mods = [with_uid | NodeKey_Module with_uid <- Set.toList all_deps]

    get_mod_info (ModNodeKeyWithUid gwib uid) =
      case lookupHug (ue_home_unit_graph unit_env) uid (gwib_mod gwib) of
        Just hmi -> do
          let iface = hm_iface hmi
          pure (LinkExternal (LinkOnlyPackages iface) (mi_module iface), hmi)
        Nothing -> throwProgramError opts $
          text "getLinkDeps: Home module not loaded" <+> ppr (gwib_mod gwib) <+> ppr uid

    no_obj :: Outputable a => a -> IO b
    no_obj mod = dieWith opts span $
                     text "cannot find object file for module" <+>
                        quotes (ppr mod) $$
                     while_linking_expr

    while_linking_expr = text "while linking an interpreted expression"

  -- Extract the 'Linkable's for unlinked modules from the intermediate
  -- results.
    module_linkable = \case
      LinkHomeModule hmi ->
        adjust_linkable (expectJust "getLinkDeps" (homeModLinkable hmi))

      LinkObjectModule mod loc -> do
        findObjectLinkableMaybe mod loc >>= \case
          Nothing  -> no_obj mod
          Just lnk -> adjust_linkable lnk

      LinkByteCodeModule _ load_bytecode ->
        load_bytecode

    -- See Note [Using Byte Code rather than Object Code for Template Haskell]
    homeModLinkable :: HomeModInfo -> Maybe Linkable
    homeModLinkable hmi =
      if ldUseByteCode opts
        then homeModInfoByteCode hmi <|> homeModInfoObject hmi
        else homeModInfoObject hmi   <|> homeModInfoByteCode hmi

    adjust_linkable lnk
        | Just new_osuf <- maybe_normal_osuf = do
                new_uls <- mapM (adjust_part (ldObjSuffix opts) new_osuf)
                                (linkableParts lnk)
                return lnk {linkableParts = new_uls}
        | otherwise =
                return lnk

    adjust_part osuf new_osuf part = case part of
      DotO file ModuleObject -> do
        massert (osuf `isSuffixOf` file)
        let file_base = fromJust (stripExtension osuf file)
            new_file = file_base <.> new_osuf
        ok <- doesFileExist new_file
        if (not ok)
            then dieWith opts span $
                  text "cannot find object file"
                        <+> quotes (text new_file) $$ while_linking_expr
            else return (DotO new_file ModuleObject)
      DotO file ForeignObject -> pure (DotO file ForeignObject)
      DotA fp    -> panic ("adjust_part DotA " ++ show fp)
      DotDLL fp  -> panic ("adjust_part DotDLL " ++ show fp)
      BCOs {}    -> pure part
      LazyBCOs{} -> pure part
      CoreBindings WholeCoreBindings {wcb_module} ->
        pprPanic "Unhydrated core bindings" (ppr wcb_module)

data LinkDep =
  LinkModules !(UniqDFM ModuleName LinkModule)
  |
  LinkLibrary !UnitId

instance Outputable LinkDep where
  ppr = \case
    LinkModules mods -> text "modules:" <+> ppr (eltsUDFM mods)
    LinkLibrary uid -> text "library:" <+> ppr uid

data OneshotError =
  NoInterface !MissingInterfaceError
  |
  LinkBootModule !Module

-- | Compute the transitive dependency closure of the given modules.
--
-- Used for all oneshot mode dependencies and for external dependencies of home
-- modules in make mode.
external_deps ::
  LinkDepsOpts ->
  -- | Modules whose imports to follow
  [LinkExternal] ->
  IO [LinkDep]
external_deps opts mods =
  runExceptT (external_deps_loop opts mods emptyUDFM) >>= \case
    Right a -> pure (eltsUDFM a)
    Left err -> throwProgramError opts (message err)
  where
    message = \case
      NoInterface err ->
        missingInterfaceErrorDiagnostic (ldMsgOpts opts) err
      LinkBootModule mod ->
        link_boot_mod_error mod

external_deps_loop ::
  LinkDepsOpts ->
  [LinkExternal] ->
  UniqDFM UnitId LinkDep ->
  ExceptT OneshotError IO (UniqDFM UnitId LinkDep)
external_deps_loop _ [] acc =
  pure acc
external_deps_loop opts (job@LinkExternal {le_module = mod, ..} : mods) acc = do
  (new_acc, new_mods, action) <-
    if already_seen
    then done
    else process_module le_details
  traverse_ debug_log action
  external_deps_loop opts (new_mods ++ mods) new_acc
  where
    debug_log action =
      liftIO $ debugTraceMsg (ldLogger opts) 3 $
      text "TH dep" <+> ppr job <+>
      brackets (sep (punctuate comma [
        if is_home then text "home" else Outputable.empty,
        text action
      ]))

    done = pure (acc, [], Nothing)

    -- Decide how this module needs to be processed.
    -- We only need an interface if we want to load bytecode or if we have to
    -- link an object file (which happens for home unit modules, since those
    -- have no libraries).
    process_module = \case
      LinkAllDeps | is_home || package_bc -> try_iface
                  | otherwise -> add_library

    -- @LinkOnlyPackages@ is used for make mode home modules, so all imports
    -- that are not external are already processed otherwise.
      LinkOnlyPackages iface -> with_deps acc iface False "only packages"

    -- Check whether the current module was processed before.
    -- Since the accumulator is keyed by unit ID, we have to perform two
    -- lookups.
    -- If another module from this module's unit has been determined to be
    -- linked as a library previously, we skip this module, assuming that no
    -- bytecode is available for the entire package.
    already_seen
      | Just (LinkModules mods) <- mod_dep
      = elemUDFM mod_name mods
      | Just (LinkLibrary _) <- mod_dep
      = True
      | otherwise
      = False

    -- Load the iface and attempt to get bytecode from Core bindings.
    try_iface =
      liftIO (ldLoadIface opts load_reason mod) >>= \case
        Failed err -> throwE (NoInterface err)
        Succeeded (iface, loc) -> do
          mb_load_bc <- liftIO (ldLoadByteCode opts (mi_module iface))
          with_iface iface loc mb_load_bc

    -- Decide how to link this module.
    -- If bytecode or an object file is available, use those in that order.
    -- Otherwise fall back to linking a library.
    with_iface iface loc mb_load_bc
      | IsBoot <- mi_boot iface
      = throwE (LinkBootModule mod)

      | ldUseByteCode opts
      , is_home || package_bc
      , Just load_bc <- mb_load_bc
      = add_module iface (LinkByteCodeModule mod load_bc) "bytecode"

      | is_home
      = add_module iface (LinkObjectModule mod loc) "object"

      | otherwise
      = add_library

    add_library =
      pure (addToUDFM acc mod_unit_id (LinkLibrary mod_unit_id), [], Just "library")

    add_module iface lmod action =
      with_deps with_mod iface True action
      where
        with_mod = alterUDFM (add_package_module lmod) acc mod_unit_id

    add_package_module lmod = \case
      Just (LinkLibrary u) -> Just (LinkLibrary u)
      Just (LinkModules old) -> Just (LinkModules (addToUDFM old mod_name lmod))
      Nothing -> Just (LinkModules (unitUDFM mod_name lmod))

    with_deps acc iface local action =
      pure (addListToUDFM acc link, new_local ++ new_package, Just action)
      where
        !(!link, !new_package) = package_deps iface
        new_local = if local then local_deps iface else []

    local_deps iface =
      [
        LinkExternal LinkAllDeps (mkModule mod_unit m)
        | (_, GWIB m _) <- Set.toList (dep_direct_mods (mi_deps iface))
      ]

    -- If bytecode linking of external dependencies is enabled, add them to the
    -- jobs passed to the next iteration of 'external_deps_loop'.
    -- Otherwise, link all package deps as libraries.
    package_deps iface
      | package_bc
      = ([], [LinkExternal LinkAllDeps m | m <- Set.toList (dep_direct_pkg_mods (mi_deps iface))])
      | otherwise
      = ([(u, LinkLibrary u) | u <- Set.toList (dep_direct_pkgs (mi_deps iface))], [])

    load_reason =
      text "need to link module" <+> ppr mod <+>
      text "due to use of Template Haskell"

    package_bc = ldPkgByteCode opts

    -- In multiple home unit mode, this only considers modules from the same
    -- unit as the splice's module to be eligible for linking bytecode when
    -- @-fpackage-db-byte-code@ is off.
    -- For make mode, this is irrelevant, since any bytecode from the HUG is
    -- obtained directly, not going through 'external_deps'.
    is_home
      | Just home <- ue_homeUnit (ldUnitEnv opts)
      = homeUnitAsUnit home == mod_unit
      | otherwise
      = False

    mod_dep = lookupUDFM acc mod_unit_id
    mod_name = moduleName mod
    mod_unit_id = moduleUnitId mod
    mod_unit = moduleUnit mod

link_boot_mod_error :: Module -> SDoc
link_boot_mod_error mod =
  text "module" <+> ppr mod <+>
  text "cannot be linked; it is only available as a boot module"

-- | Split link dependencies into the sets of modules and packages that have
-- been linked previously and those that need to be linked now by checking for
-- their presence in the 'LoaderState':
--
-- - For module dependencies, in the sets of loaded objects and BCOs
--   ('objs_loaded' and 'bcos_loaded')
-- - For package dependencies, in the set of loaded packages ('pkgs_loaded')
classify_deps ::
  LoaderState ->
  [HomeModInfo] ->
  [LinkDep] ->
  ([Linkable], [LinkModule], UniqDSet UnitId, [UnitId])
classify_deps pls hmis deps =
  (loaded_modules' ++ loaded_modules'', needed_modules' ++ needed_modules'', all_packages, needed_packages)
  where
    (loaded_modules', needed_modules') = partitionWith loaded_or_needed_home_module hmis
    (loaded_modules'', needed_modules'') =
      partitionWith loaded_or_needed_module (concatMap eltsUDFM modules)

    needed_packages =
      eltsUDFM (getUniqDSet all_packages `minusUDFM` pkgs_loaded pls)

    all_packages = mkUniqDSet packages

    (modules, packages) = flip partitionWith deps $ \case
      LinkModules mods -> Left mods
      LinkLibrary lib -> Right lib

    loaded_or_needed_home_module lm =
      maybe (Right (LinkHomeModule lm)) Left (loaded_module (mi_module (hm_iface lm)))

    loaded_or_needed_module lm =
      maybe (Right lm) Left (loaded_module (link_module lm))

    loaded_module mod =
      lookupModuleEnv (objs_loaded pls) mod
      <|>
      lookupModuleEnv (bcos_loaded pls) mod

{-
Note [Using Byte Code rather than Object Code for Template Haskell]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The `-fprefer-byte-code` flag allows a user to specify that they want to use
byte code (if available) rather than object code for home module dependencies
when executing Template Haskell splices.

Why might you want to use byte code rather than object code?

* Producing object code is much slower than producing byte code (for example if you're using -fno-code)
* Linking many large object files, which happens once per splice, is quite expensive. (#21700)

So we allow the user to choose to use byte code rather than object files if they want to avoid these
two pitfalls.

When using `-fprefer-byte-code` you have to arrange to have the byte code available.
In normal --make mode it will not be produced unless you enable `-fbyte-code-and-object-code`.
See Note [Home module build products] for some more information about that.

The only other place where the flag is consulted is when enabling code generation
with `-fno-code`, which does so to anticipate what decision we will make at the
splice point about what we would prefer.

-}

dieWith :: LinkDepsOpts -> SrcSpan -> SDoc -> IO a
dieWith opts span msg = throwProgramError opts (mkLocMessage MCFatal span msg)

throwProgramError :: LinkDepsOpts -> SDoc -> IO a
throwProgramError opts doc = throwGhcExceptionIO (ProgramError (renderWithContext (ldPprOpts opts) doc))

checkNonStdWay :: LinkDepsOpts -> Interp -> SrcSpan -> IO (Maybe FilePath)
checkNonStdWay _opts interp _srcspan
  | ExternalInterp {} <- interpInstance interp = return Nothing
    -- with -fexternal-interpreter we load the .o files, whatever way
    -- they were built.  If they were built for a non-std way, then
    -- we will use the appropriate variant of the iserv binary to load them.

-- #if-guard the following equations otherwise the pattern match checker will
-- complain that they are redundant.
#if defined(HAVE_INTERNAL_INTERPRETER)
checkNonStdWay opts _interp srcspan
  | hostFullWays == targetFullWays = return Nothing
    -- Only if we are compiling with the same ways as GHC is built
    -- with, can we dynamically load those object files. (see #3604)

  | ldObjSuffix opts == normalObjectSuffix && not (null targetFullWays)
  = failNonStd opts srcspan

  | otherwise = return (Just (hostWayTag ++ "o"))
  where
    targetFullWays = fullWays (ldWays opts)
    hostWayTag = case waysTag hostFullWays of
                  "" -> ""
                  tag -> tag ++ "_"

    normalObjectSuffix :: String
    normalObjectSuffix = "o"

data Way' = Normal | Prof | Dyn | ProfDyn

failNonStd :: LinkDepsOpts -> SrcSpan -> IO (Maybe FilePath)
failNonStd opts srcspan = dieWith opts srcspan $
  text "Cannot load" <+> pprWay' compWay <+>
     text "objects when GHC is built" <+> pprWay' ghciWay $$
  text "To fix this, either:" $$
  text "  (1) Use -fexternal-interpreter, or" $$
  buildTwiceMsg
    where compWay
            | ldWays opts `hasWay` WayDyn && ldWays opts `hasWay` WayProf = ProfDyn
            | ldWays opts `hasWay` WayDyn  = Dyn
            | ldWays opts `hasWay` WayProf = Prof
            | otherwise = Normal
          ghciWay
            | hostIsDynamic && hostIsProfiled = ProfDyn
            | hostIsDynamic = Dyn
            | hostIsProfiled = Prof
            | otherwise = Normal
          buildTwiceMsg = case (ghciWay, compWay) of
            (Normal, Dyn) -> dynamicTooMsg
            (Dyn, Normal) -> dynamicTooMsg
            _ ->
              text "  (2) Build the program twice: once" <+>
                pprWay' ghciWay <> text ", and then" $$
              text "      " <> pprWay' compWay <+>
                text "using -osuf to set a different object file suffix."
          dynamicTooMsg = text "  (2) Use -dynamic-too," <+>
            text "and use -osuf and -dynosuf to set object file suffixes as needed."
          pprWay' :: Way' -> SDoc
          pprWay' way = text $ case way of
            Normal -> "the normal way"
            Prof -> "with -prof"
            Dyn -> "with -dynamic"
            ProfDyn -> "with -prof and -dynamic"
#endif
