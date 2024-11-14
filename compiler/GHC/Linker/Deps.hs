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

import GHC.Types.SourceFile
import GHC.Types.SrcLoc
import GHC.Types.Unique.DSet
import GHC.Types.Unique.DFM

import GHC.Utils.Outputable
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

import qualified Data.Set as Set
import qualified Data.Map as M
import Data.List (isSuffixOf)

import System.FilePath
import System.Directory
import GHC.Driver.Env
import {-# SOURCE #-} GHC.Driver.Main
import Data.Time.Clock


data LinkDepsOpts = LinkDepsOpts
  { ldObjSuffix   :: !String                        -- ^ Suffix of .o files
  , ldOneShotMode :: !Bool                          -- ^ Is the driver in one-shot mode?
  , ldModuleGraph :: !ModuleGraph                   -- ^ Module graph
  , ldUnitEnv     :: !UnitEnv                       -- ^ Unit environment
  , ldPprOpts     :: !SDocContext                   -- ^ Rendering options for error messages
  , ldFinderCache :: !FinderCache                   -- ^ Finder cache
  , ldFinderOpts  :: !FinderOpts                    -- ^ Finder options
  , ldHugFinderOpts :: !(UnitEnvGraph FinderOpts)
  , ldUseByteCode :: !Bool                          -- ^ Use bytecode rather than objects
  , ldMsgOpts     :: !(DiagnosticOpts IfaceMessage) -- ^ Options for diagnostics
  , ldWays        :: !Ways                          -- ^ Enabled ways
  , ldLoadIface   :: SDoc -> Module -> IO (MaybeErr MissingInterfaceError ModIface)
                                                    -- ^ Interface loader function
  , ldHscEnv      :: !HscEnv
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

      get_link_deps opts pls maybe_normal_osuf span mods

get_link_deps
  :: LinkDepsOpts
  -> LoaderState
  -> Maybe FilePath  -- replace object suffixes?
  -> SrcSpan
  -> [Module]
  -> IO LinkDeps
get_link_deps opts pls maybe_normal_osuf span mods = do
  -- 1.  Find the dependent home-pkg-modules/packages from each iface
  --     (omitting modules from the interactive package, which is already linked)
  --     Why two code paths here? There is a significant amount of repeated work
  --     performed calculating transitive dependencies
  --     if --make uses the oneShot code path (see MultiLayerModulesTH_* tests)
  deps <- if ldOneShotMode opts
          then oneshot_deps opts (filterOut isInteractiveModule mods)
          else make_deps

  -- 2.  Exclude ones already linked
  --     Main reason: avoid findModule calls in get_linkable
  -- TODO outdated
  let (loaded_modules, needed_modules, ldAllUnits, ldNeededUnits) =
        classify_deps pls deps

  -- 3.  For each dependent module, find its linkable
  --     This will either be in the HPT or (in the case of one-shot
  --     compilation) we may need to use maybe_getFileLinkable
  -- TODO outdated
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

    make_deps = do
      (pkgs, mmods) <- unzip <$> mapM get_mod_info all_home_mods
      let
        link_mods =
          listToUDFM [(moduleName (mi_module (hm_iface m)), m) | m <- mmods]
        link_libs =
          uniqDSetToList (unionManyUniqDSets (init_pkg_set : pkgs))
      pure $
        LinkModules (LinkHomeModule <$> link_mods) :
        (LinkLibrary <$> link_libs)

    -- This code is used in `--make` mode to calculate the home package and unit dependencies
    -- for a set of modules.
    --
    -- It is significantly more efficient to use the shared transitive dependency
    -- calculation than to compute the transitive dependency set in the same manner as oneShot mode.

    -- It is also a matter of correctness to use the module graph so that dependencies between home units
    -- is resolved correctly.
    make_deps_loop :: (UniqDSet UnitId, Set.Set NodeKey) -> [ModNodeKeyWithUid] -> (UniqDSet UnitId, Set.Set NodeKey)
    make_deps_loop found [] = found
    make_deps_loop found@(found_units, found_mods) (nk:nexts)
      | NodeKey_Module nk `Set.member` found_mods = make_deps_loop found nexts
      | otherwise =
        case fmap mkNodeKey <$> mgReachable mod_graph (NodeKey_Module nk) of
          Nothing ->
              let (ModNodeKeyWithUid _ uid) = nk
              in make_deps_loop (addOneToUniqDSet found_units uid, found_mods) nexts
          Just trans_deps ->
            let deps = Set.insert (NodeKey_Module nk) (Set.fromList trans_deps)
                -- See #936 and the ghci.prog007 test for why we have to continue traversing through
                -- boot modules.
                todo_boot_mods = [ModNodeKeyWithUid (GWIB mn NotBoot) uid | NodeKey_Module (ModNodeKeyWithUid (GWIB mn IsBoot) uid) <- trans_deps]
            in make_deps_loop (found_units, deps `Set.union` found_mods) (todo_boot_mods ++ nexts)

    mkNk m = ModNodeKeyWithUid (GWIB (moduleName m) NotBoot) (moduleUnitId m)
    (init_pkg_set, all_deps) = make_deps_loop (emptyUniqDSet, Set.empty) $ map mkNk (filterOut isInteractiveModule mods)

    all_home_mods = [with_uid | NodeKey_Module with_uid <- Set.toList all_deps]

    get_mod_info (ModNodeKeyWithUid gwib uid) =
      case lookupHug (ue_home_unit_graph unit_env) uid (gwib_mod gwib) of
        Just hmi -> do
          let iface = hm_iface hmi
          case mi_hsc_src iface of
            HsBootFile -> throwProgramError opts $ link_boot_mod_error (mi_module iface)
            _ -> pure (mkUniqDSet $ Set.toList $ dep_direct_pkgs (mi_deps iface), hmi)
        Nothing -> throwProgramError opts $
          text "getLinkDeps: Home module not loaded" <+> ppr (gwib_mod gwib) <+> ppr uid

    no_obj :: Outputable a => a -> IO b
    no_obj mod = dieWith opts span $
                     text "cannot find object file for module " <>
                        quotes (ppr mod) $$
                     while_linking_expr

    while_linking_expr = text "while linking an interpreted expression"

    module_linkable = \case
      LinkHomeModule hmi ->
        adjust_linkable (expectJust "getLinkDeps" (homeModLinkable hmi))

      LinkObjectModule iface loc -> do
        let mod = mi_module iface
        findObjectLinkableMaybe mod loc >>= \case
          Nothing  -> no_obj mod
          Just lnk -> adjust_linkable lnk

      LinkByteCodeModule iface wcb -> do
        details <- initModDetails (ldHscEnv opts) iface
        t <- getCurrentTime
        initWholeCoreBindings (ldHscEnv opts) iface details $ LM t (mi_module iface) [CoreBindings wcb]

    -- See Note [Using Byte Code rather than Object Code for Template Haskell]
    homeModLinkable :: HomeModInfo -> Maybe Linkable
    homeModLinkable hmi =
      if ldUseByteCode opts
        then homeModInfoByteCode hmi <|> homeModInfoObject hmi
        else homeModInfoObject hmi   <|> homeModInfoByteCode hmi

    adjust_linkable lnk
        | Just new_osuf <- maybe_normal_osuf = do
                new_uls <- mapM (adjust_ul new_osuf)
                                (linkableUnlinked lnk)
                return lnk{ linkableUnlinked=new_uls }
        | otherwise =
                return lnk

    adjust_ul new_osuf (DotO file) = do
        -- file may already has new_osuf suffix. One example
        -- is when we load bytecode from whole core bindings,
        -- then the corresponding foreign stub objects are
        -- compiled as shared objects and file may already has
        -- .dyn_o suffix. And it's okay as long as the file to
        -- load is already there.
        let new_file = file -<.> new_osuf
        ok <- doesFileExist new_file
        if (not ok)
            then dieWith opts span $
                  text "cannot find object file "
                        <> quotes (text new_file) $$ while_linking_expr
            else return (DotO new_file)
    adjust_ul _ (DotA fp) = panic ("adjust_ul DotA " ++ show fp)
    adjust_ul _ (DotDLL fp) = panic ("adjust_ul DotDLL " ++ show fp)
    adjust_ul _ l@(BCOs {}) = return l
    adjust_ul _ l@LoadedBCOs{} = return l
    adjust_ul _ (CoreBindings (WholeCoreBindings _ mod _))     = pprPanic "Unhydrated core bindings" (ppr mod)

data LinkModule =
  LinkHomeModule HomeModInfo
  |
  LinkObjectModule ModIface ModLocation
  |
  LinkByteCodeModule ModIface WholeCoreBindings

link_module_iface :: LinkModule -> ModIface
link_module_iface = \case
  LinkHomeModule hmi -> hm_iface hmi
  LinkObjectModule iface _ -> iface
  LinkByteCodeModule iface _ -> iface

instance Outputable LinkModule where
  ppr = \case
    LinkHomeModule hmi -> ppr (mi_module (hm_iface hmi)) <+> brackets (text "HMI")
    LinkObjectModule iface _ -> ppr (mi_module iface)
    LinkByteCodeModule _ wcb -> ppr (wcb_module wcb) <+> brackets (text "BC")

data LinkDep =
  LinkModules (UniqDFM ModuleName LinkModule)
  |
  LinkLibrary UnitId

instance Outputable LinkDep where
  ppr = \case
    LinkModules mods -> text "modules:" <+> ppr (eltsUDFM mods)
    LinkLibrary uid -> text "library:" <+> ppr uid

data OneshotError =
  NoLocation Module
  |
  NoInterface MissingInterfaceError
  |
  LinkBootModule Module

-- This code is used in one-shot mode to traverse downwards through the HPT
-- to find all link dependencies.
-- The ModIface contains the transitive closure of the module dependencies
-- within the current package, *except* for boot modules: if we encounter
-- a boot module, we have to find its real interface and discover the
-- dependencies of that.  Hence we need to traverse the dependency
-- tree recursively.  See bug #936, testcase ghci/prog007.
oneshot_deps ::
  LinkDepsOpts ->
  -- | Modules whose imports to follow
  [Module] ->
  IO [LinkDep]
oneshot_deps opts mods =
  runExceptT (oneshot_deps_loop opts mods emptyUDFM) >>= \case
    Right a -> pure (eltsUDFM a)
    Left err -> throwProgramError opts (message err)
  where
    message = \case
      NoLocation mod ->
        pprPanic "found iface but no location" (ppr mod)
      NoInterface err ->
        missingInterfaceErrorDiagnostic (ldMsgOpts opts) err
      LinkBootModule mod ->
        link_boot_mod_error mod

oneshot_deps_loop ::
  LinkDepsOpts ->
  [Module] ->
  UniqDFM UnitId LinkDep ->
  ExceptT OneshotError IO (UniqDFM UnitId LinkDep)
oneshot_deps_loop _ [] acc =
  pure acc
oneshot_deps_loop opts (mod : mods) acc = do
  (new_acc, new_mods) <- process_module
  oneshot_deps_loop opts (new_mods ++ mods) new_acc
  where
    process_module
      | already_seen = pure (acc, [])
      | is_home || bytecode = try_iface
      | otherwise = add_library

    already_seen
      | Just (LinkModules mods) <- mod_dep
      = elemUDFM mod_name mods
      | Just (LinkLibrary _) <- mod_dep
      = True
      | otherwise
      = False

    try_iface =
      liftIO (ldLoadIface opts load_reason mod) >>= \case
        Failed err -> throwE (NoInterface err)
        Succeeded iface ->
          location >>= \case
            InstalledFound loc _ -> with_iface loc iface
            _ -> throwE (NoLocation mod)

    with_iface loc iface
      | mi_boot iface == IsBoot
      = throwE (LinkBootModule mod)
      | bytecode
      , Just core_bindings <- mi_extra_decls iface
      , let wcb = WholeCoreBindings core_bindings mod loc
      = pure (add_module iface (LinkByteCodeModule iface wcb))
      | is_home
      = pure (add_module iface (LinkObjectModule iface loc))
      | otherwise
      = add_library

    add_library = pure (addToUDFM acc mod_unit_id (LinkLibrary mod_unit_id), [])

    add_module iface lmod =
      (addListToUDFM with_mod (direct_pkgs iface), new_deps iface)
      where
        with_mod = alterUDFM (add_package_module lmod) acc mod_unit_id

    add_package_module lmod = \case
      Just (LinkLibrary u) -> Just (LinkLibrary u)
      Just (LinkModules old) -> Just (LinkModules (addToUDFM old mod_name lmod))
      Nothing -> Just (LinkModules (unitUDFM mod_name lmod))

    direct_pkgs iface
      | bytecode
      = []
      | otherwise
      = [(u, LinkLibrary u) | u <- Set.toList (dep_direct_pkgs (mi_deps iface))]

    new_deps iface
      | bytecode
      -- TODO How can we better determine the external deps?
      = [usg_mod | UsagePackageModule {usg_mod} <- mi_usages iface] ++ local
      | is_home
      = local
      | otherwise
      = []
      where
        local =
          [
            mkModule mod_unit m
            -- TODO Somehow this just works, no idea what the deal was in the
            -- old code with boot modules.
            | (_, GWIB m _) <- Set.toList (dep_direct_mods (mi_deps iface))
          ]

    is_home
      | Just home <- mb_home
      = homeUnitAsUnit home == mod_unit
      | otherwise
      = False

    location =
      liftIO $
      findExactModule (ldFinderCache opts) (ldFinderOpts opts)
      (ldHugFinderOpts opts) (hsc_units (ldHscEnv opts)) mb_home
      (toUnitId <$> mod)

    mod_dep = lookupUDFM acc mod_unit_id
    mod_name = moduleName mod
    mod_unit_id = moduleUnitId mod
    mod_unit = moduleUnit mod
    load_reason =
      text "need to link module" <+> ppr mod <+>
      text "due to use of Template Haskell"

    bytecode = ldUseByteCode opts
    mb_home = ue_homeUnit (ldUnitEnv opts)

link_boot_mod_error :: Module -> SDoc
link_boot_mod_error mod =
  text "module" <+> ppr mod <+>
  text "cannot be linked; it is only available as a boot module"

classify_deps ::
  LoaderState ->
  [LinkDep] ->
  ([Linkable], [LinkModule], UniqDSet UnitId, [UnitId])
classify_deps pls deps =
  (loaded_modules, needed_modules, all_packages, needed_packages)
  where
    (loaded_modules, needed_modules) =
      partitionWith loaded_or_needed (concatMap eltsUDFM modules)

    needed_packages =
      eltsUDFM (getUniqDSet all_packages `minusUDFM` pkgs_loaded pls)

    all_packages = mkUniqDSet packages

    (modules, packages) = flip partitionWith deps $ \case
      LinkModules mods -> Left mods
      LinkLibrary lib -> Right lib

    loaded_or_needed lm =
      maybe (Right lm) Left (loaded_linkable (mi_module (link_module_iface lm)))

    loaded_linkable mod =
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

data Way' = Normal | Prof | Dyn

failNonStd :: LinkDepsOpts -> SrcSpan -> IO (Maybe FilePath)
failNonStd opts srcspan = dieWith opts srcspan $
  text "Cannot load" <+> pprWay' compWay <+>
     text "objects when GHC is built" <+> pprWay' ghciWay $$
  text "To fix this, either:" $$
  text "  (1) Use -fexternal-interpreter, or" $$
  buildTwiceMsg
    where compWay
            | ldWays opts `hasWay` WayDyn  = Dyn
            | ldWays opts `hasWay` WayProf = Prof
            | otherwise = Normal
          ghciWay
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
#endif

