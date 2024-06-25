-- The transition from Int to Word64 for uniques makes functions slightly larger
-- without this GHC option some optimizations fail to fire.
-- See https://gitlab.haskell.org/ghc/ghc/-/merge_requests/10568#note_505751
{-# OPTIONS_GHC -fspec-constr-threshold=10000 #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

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

import Control.Monad
import Control.Applicative

import qualified Data.Set as Set
import qualified Data.Map as M
import Data.List (isSuffixOf)

import System.FilePath
import System.Directory
import GHC.Driver.Env
import {-# SOURCE #-} GHC.Driver.Main
import Data.Time.Clock
import GHC.Driver.Flags
import GHC.Driver.Session

data LinkDepsOpts = LinkDepsOpts
  { ldObjSuffix   :: !String                        -- ^ Suffix of .o files
  , ldOneShotMode :: !Bool                          -- ^ Is the driver in one-shot mode?
  , ldModuleGraph :: !ModuleGraph                   -- ^ Module graph
  , ldUnitEnv     :: !UnitEnv                       -- ^ Unit environment
  , ldPprOpts     :: !SDocContext                   -- ^ Rendering options for error messages
  , ldFinderCache :: !FinderCache                   -- ^ Finder cache
  , ldFinderOpts  :: !FinderOpts                    -- ^ Finder options
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
  , ldUnits           :: [UnitId]
  , ldNeededUnits     :: UniqDSet UnitId
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
        -- (omitting modules from the interactive package, which is already linked)
      (mods_s, pkgs_s) <-
          -- Why two code paths here? There is a significant amount of repeated work
          -- performed calculating transitive dependencies
          -- if --make uses the oneShot code path (see MultiLayerModulesTH_* tests)
          if ldOneShotMode opts
            then follow_deps (filterOut isInteractiveModule mods)
                              emptyUniqDSet emptyUniqDSet;
            else do
              (pkgs, mmods) <- unzip <$> mapM get_mod_info all_home_mods
              return (catMaybes mmods, unionManyUniqDSets (init_pkg_set : pkgs))

      let
        -- 2.  Exclude ones already linked
        --      Main reason: avoid findModule calls in get_linkable
            (mods_needed, links_got) = partitionWith split_mods mods_s
            pkgs_needed = eltsUDFM $ getUniqDSet pkgs_s `minusUDFM` pkgs_loaded pls

            split_mods mod =
                let is_linked = lookupModuleEnv (objs_loaded pls) mod
                                <|> lookupModuleEnv (bcos_loaded pls) mod
                in case is_linked of
                     Just linkable -> Right linkable
                     Nothing -> Left mod

        -- 3.  For each dependent module, find its linkable
        --     This will either be in the HPT or (in the case of one-shot
        --     compilation) we may need to use maybe_getFileLinkable
      lnks_needed <- mapM (get_linkable (ldObjSuffix opts)) mods_needed

      return $ LinkDeps
        { ldNeededLinkables = lnks_needed
        , ldAllLinkables    = links_got ++ lnks_needed
        , ldUnits           = pkgs_needed
        , ldNeededUnits     = pkgs_s
        }
  where
    mod_graph = ldModuleGraph opts
    unit_env  = ldUnitEnv     opts

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
        case M.lookup (NodeKey_Module nk) (mgTransDeps mod_graph) of
            Just trans_deps ->
              let deps = Set.insert (NodeKey_Module nk) trans_deps
                  -- See #936 and the ghci.prog007 test for why we have to continue traversing through
                  -- boot modules.
                  todo_boot_mods = [ModNodeKeyWithUid (GWIB mn NotBoot) uid | NodeKey_Module (ModNodeKeyWithUid (GWIB mn IsBoot) uid) <- Set.toList trans_deps]
              in make_deps_loop (found_units, deps `Set.union` found_mods) (todo_boot_mods ++ nexts)
            Nothing ->
              let (ModNodeKeyWithUid _ uid) = nk
              in make_deps_loop (addOneToUniqDSet found_units uid, found_mods) nexts

    mkNk m = ModNodeKeyWithUid (GWIB (moduleName m) NotBoot) (moduleUnitId m)
    (init_pkg_set, all_deps) = make_deps_loop (emptyUniqDSet, Set.empty) $ map mkNk (filterOut isInteractiveModule mods)

    all_home_mods = [with_uid | NodeKey_Module with_uid <- Set.toList all_deps]

    get_mod_info (ModNodeKeyWithUid gwib uid) =
      case lookupHug (ue_home_unit_graph unit_env) uid (gwib_mod gwib) of
        Just hmi ->
          let iface = (hm_iface hmi)
              mmod = case mi_hsc_src iface of
                      HsBootFile -> link_boot_mod_error (mi_module iface)
                      _          -> return $ Just (mi_module iface)

          in (mkUniqDSet $ Set.toList $ dep_direct_pkgs (mi_deps iface),) <$>  mmod
        Nothing -> throwProgramError opts $
          text "getLinkDeps: Home module not loaded" <+> ppr (gwib_mod gwib) <+> ppr uid


       -- This code is used in one-shot mode to traverse downwards through the HPT
       -- to find all link dependencies.
       -- The ModIface contains the transitive closure of the module dependencies
       -- within the current package, *except* for boot modules: if we encounter
       -- a boot module, we have to find its real interface and discover the
       -- dependencies of that.  Hence we need to traverse the dependency
       -- tree recursively.  See bug #936, testcase ghci/prog007.
    follow_deps :: [Module]             -- modules to follow
                -> UniqDSet Module         -- accum. module dependencies
                -> UniqDSet UnitId          -- accum. package dependencies
                -> IO ([Module], UniqDSet UnitId) -- result
    follow_deps []     acc_mods acc_pkgs
        = return (uniqDSetToList acc_mods, acc_pkgs)
    follow_deps (mod:mods) acc_mods acc_pkgs
        = do
          mb_iface <- ldLoadIface opts msg mod
          iface <- case mb_iface of
                    Failed err      -> throwProgramError opts $
                      missingInterfaceErrorDiagnostic (ldMsgOpts opts) err
                    Succeeded iface -> return iface

          when (mi_boot iface == IsBoot) $ link_boot_mod_error mod

          let
            pkg = moduleUnit mod
            deps  = mi_deps iface

            pkg_deps = dep_direct_pkgs deps
            (boot_deps, mod_deps) = flip partitionWith (Set.toList (dep_direct_mods deps)) $
              \case
                (_, GWIB m IsBoot)  -> Left m
                (_, GWIB m NotBoot) -> Right m

            mod_deps' = case ue_homeUnit unit_env of
                          Nothing -> []
                          Just home_unit -> filter (not . (`elementOfUniqDSet` acc_mods)) (map (mkHomeModule home_unit) $ (boot_deps ++ mod_deps))
            acc_mods'  = case ue_homeUnit unit_env of
                          Nothing -> acc_mods
                          Just home_unit -> addListToUniqDSet acc_mods (mod : map (mkHomeModule home_unit) mod_deps)
            acc_pkgs'  = addListToUniqDSet acc_pkgs (Set.toList pkg_deps)

          case ue_homeUnit unit_env of
            Just home_unit | isHomeUnit home_unit pkg ->  follow_deps (mod_deps' ++ mods)
                                                                      acc_mods' acc_pkgs'
            _ ->  follow_deps mods acc_mods (addOneToUniqDSet acc_pkgs' (toUnitId pkg))
        where
           msg = text "need to link module" <+> ppr mod <+>
                  text "due to use of Template Haskell"



    link_boot_mod_error :: Module -> IO a
    link_boot_mod_error mod = throwProgramError opts $
            text "module" <+> ppr mod <+>
            text "cannot be linked; it is only available as a boot module"

    no_obj :: Outputable a => a -> IO b
    no_obj mod = dieWith opts span $
                     text "cannot find object file for module " <>
                        quotes (ppr mod) $$
                     while_linking_expr

    while_linking_expr = text "while linking an interpreted expression"


    -- See Note [Using Byte Code rather than Object Code for Template Haskell]
    homeModLinkable :: HomeModInfo -> Maybe Linkable
    homeModLinkable hmi =
      if ldUseByteCode opts
        then homeModInfoByteCode hmi <|> homeModInfoObject hmi
        else homeModInfoObject hmi   <|> homeModInfoByteCode hmi

    get_linkable osuf mod      -- A home-package module
        | Just mod_info <- lookupHugByModule mod (ue_home_unit_graph unit_env)
        = adjust_linkable (expectJust "getLinkDeps" (homeModLinkable mod_info))
        | otherwise
        = do    -- It's not in the HPT because we are in one shot mode,
                -- so use the Finder to get a ModLocation...
             case ue_homeUnit unit_env of
              Nothing -> no_obj mod
              Just home_unit -> do

                let fc = ldFinderCache opts
                let fopts = ldFinderOpts opts
                mb_stuff <- findHomeModule fc fopts home_unit (moduleName mod)
                case mb_stuff of
                  Found loc mod -> found loc mod
                  _ -> no_obj (moduleName mod)
        where
            found loc mod
              | prefer_bytecode = do
                  Succeeded iface <- ldLoadIface opts (text "load core bindings") mod
                  case mi_extra_decls iface of
                    Just extra_decls -> do
                      t <- getCurrentTime
                      let
                        stubs = mi_foreign iface
                        wcb = WholeCoreBindings extra_decls mod loc stubs
                        linkable = Linkable t mod (pure (CoreBindings wcb))
                      initWholeCoreBindingsEps hsc_env iface linkable
                    _ -> fallback_no_bytecode loc mod
              | otherwise = fallback_no_bytecode loc mod

            fallback_no_bytecode loc mod = do
              mb_lnk <- findObjectLinkableMaybe mod loc
              case mb_lnk of
                Nothing  -> no_obj mod
                Just lnk -> adjust_linkable lnk

            prefer_bytecode = gopt Opt_UseBytecodeRatherThanObjects dflags

            dflags = hsc_dflags hsc_env

            hsc_env = ldHscEnv opts

            adjust_linkable lnk
                | Just new_osuf <- maybe_normal_osuf = do
                        new_parts <- mapM (adjust_part new_osuf)
                                        (linkableParts lnk)
                        return lnk{ linkableParts=new_parts }
                | otherwise =
                        return lnk

            adjust_part new_osuf part = case part of
              DotO file ModuleObject -> do
                massert (osuf `isSuffixOf` file)
                let file_base = fromJust (stripExtension osuf file)
                    new_file = file_base <.> new_osuf
                ok <- doesFileExist new_file
                if (not ok)
                   then dieWith opts span $
                          text "cannot find object file "
                                <> quotes (text new_file) $$ while_linking_expr
                   else return (DotO new_file ModuleObject)
              DotO file ForeignObject -> pure (DotO file ForeignObject)
              DotA fp    -> panic ("adjust_ul DotA " ++ show fp)
              DotDLL fp  -> panic ("adjust_ul DotDLL " ++ show fp)
              BCOs {}    -> pure part
              LazyBCOs{} -> pure part
              CoreBindings WholeCoreBindings {wcb_module} ->
                pprPanic "Unhydrated core bindings" (ppr wcb_module)

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
