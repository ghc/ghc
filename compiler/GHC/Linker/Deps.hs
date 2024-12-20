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
import GHC.Unit.Module.External.Graph
import GHC.Unit.Home.ModInfo

import GHC.Iface.Errors.Types

import GHC.Utils.Misc
import GHC.Unit.Home
import GHC.Data.Maybe

import Control.Applicative

import qualified Data.Set as Set
import Data.List (isSuffixOf)

import System.FilePath
import System.Directory
import GHC.Utils.Monad (mapMaybeM)
import Data.Either (partitionEithers)
import Data.Bifunctor (Bifunctor(..))

data LinkDepsOpts = LinkDepsOpts
  { ldObjSuffix   :: !String                        -- ^ Suffix of .o files
  , ldForceDyn    :: !Bool                          -- ^ Always use .dyn_o?
  , ldOneShotMode :: !Bool                          -- ^ Is the driver in one-shot mode?
  , ldModuleGraph :: !ModuleGraph
  , ldUnitEnv     :: !UnitEnv
  , ldPprOpts     :: !SDocContext                   -- ^ Rendering options for error messages
  , ldUseByteCode :: !Bool                          -- ^ Use bytecode rather than objects
  , ldMsgOpts     :: !(DiagnosticOpts IfaceMessage) -- ^ Options for diagnostics
  , ldWays        :: !Ways                          -- ^ Enabled ways
  , ldFinderCache :: !FinderCache
  , ldFinderOpts  :: !FinderOpts
  , ldLoadByteCode :: !(Module -> IO (Maybe Linkable))
  , ldLoadHomeIfacesBelow :: !((Module -> SDoc) -> [Module] -> IO ExternalModuleGraph)
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

      -- Three step process:

        -- 1. Find the dependent home-pkg-modules/packages from each iface
        -- (omitting modules from the interactive package, which is already linked)
      (all_home_mods, pkgs_s) <- get_reachable_nodes opts relevant_mods
      mods_s <- mapMaybeM get_mod_info all_home_mods

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
    unit_env  = ldUnitEnv     opts

    relevant_mods = filterOut isInteractiveModule mods

    get_mod_info (ModNodeKeyWithUid gwib uid) =
      case lookupHug (ue_home_unit_graph unit_env) uid (gwib_mod gwib) of
        Just hmi ->
          let iface = (hm_iface hmi)
          in case mi_hsc_src iface of
              HsBootFile -> link_boot_mod_error (mi_module iface)
              _          -> return $ Just (mi_module iface)
        Nothing -> throwProgramError opts $
          text "getLinkDeps: Home module not loaded" <+> ppr (gwib_mod gwib) <+> ppr uid

    link_boot_mod_error mod = throwProgramError opts $
            text "module" <+> ppr mod <+>
            text "cannot be linked; it is only available as a boot module"


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
          iface <- _loadIface opts msg mod

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
                from_bc <- ldLoadByteCode opts mod
                maybe (fallback_no_bytecode home_unit mod) pure from_bc
        where

            fallback_no_bytecode home_unit mod = do
              let fc = ldFinderCache opts
              let fopts = ldFinderOpts opts
              mb_stuff <- findHomeModule fc fopts home_unit (moduleName mod)
              case mb_stuff of
                Found loc _ -> do
                  mb_lnk <- findObjectLinkableMaybe mod loc
                  case mb_lnk of
                    Nothing  -> no_obj mod
                    Just lnk -> adjust_linkable lnk
                _ -> no_obj (moduleName mod)

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

-- See Note [Reachability in One-shot mode vs Make mode]
get_reachable_nodes :: LinkDepsOpts -> [Module] -> IO ([ModNodeKeyWithUid], UniqDSet UnitId)
get_reachable_nodes opts mods

  -- Reachability on 'ExternalModuleGraph' (for one shot mode)
  | ldOneShotMode opts
  = do
    emg <- ldLoadHomeIfacesBelow opts msg mods
    go (ExternalModuleKey . mkModuleNk) emgNodeKey (emgReachableMany emg) (map emgProject)

  -- Reachability on 'ModuleGraph' (for --make mode)
  | otherwise
  = go hmgModKey mkNodeKey (mgReachableLoop hmGraph) (catMaybes . map hmgProject)

  where
    mkModuleNk m = ModNodeKeyWithUid (GWIB (moduleName m) NotBoot) (moduleUnitId m)
    msg mod =
      text "need to link module" <+> ppr mod <+>
        text "and the modules below it, due to use of Template Haskell"

    hmGraph = ldModuleGraph opts

    hmgModKey m
      | let k = NodeKey_Module (mkModuleNk m)
      , mgMember hmGraph k = k
      | otherwise = NodeKey_ExternalUnit (moduleUnitId m)

    hmgProject = \case
      NodeKey_Module with_uid  -> Just $ Left  with_uid
      NodeKey_ExternalUnit uid -> Just $ Right uid
      _                        -> Nothing

    emgProject = \case
      ExternalModuleKey with_uid -> Left  with_uid
      ExternalPackageKey uid     -> Right uid

    -- The main driver for getting dependencies, which calls the given
    -- functions to compute the reachable nodes.
    go :: (Module -> key)
       -> (node -> key)
       -> ([key] -> [node])
       -> ([key] -> [Either ModNodeKeyWithUid UnitId])
       -> IO ([ModNodeKeyWithUid], UniqDSet UnitId)
    go modKey nodeKey manyReachable project
      | let mod_keys = map modKey mods
      = pure $ second mkUniqDSet $ partitionEithers $ project $
          mod_keys ++ map nodeKey (manyReachable mod_keys)

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

Note [Reachability in One-shot mode vs Make mode]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Why are there two code paths in `get_reachable_nodes`? (ldOneShotMode vs otherwise)

In one-shot mode, the home package modules are loaded into the EPS,
whereas for --make mode, the home package modules are in the HUG/HPT.

For both of these cases, we cache the calculation of transitive
dependencies in a 'ModuleGraph'. For the --make case, the relevant
'ModuleGraph' is in the EPS, the other case uses the 'ModuleGraph'
for the home modules.

The home modules graph is known statically after downsweep.
On the contrary, the EPS module graph is only extended when a
module is loaded into the EPS -- which is done lazily as needed.
Therefore, for get_link_deps, we need to force the transitive
closure to be loaded before querying the graph for the reachable
link dependencies -- done in the call to 'ldLoadHomeIfacesBelow'.
Because we cache the transitive closure, this work is only done once.

After forcing the modules with the call to 'ldLoadHomeIfacesBelow' in
'get_reachable_nodes', the external module graph has all edges needed to
compute the full transitive closure so we can proceed just like we do in the
second path with a normal module graph.
-}

dieWith :: LinkDepsOpts -> SrcSpan -> SDoc -> IO a
dieWith opts span msg = throwProgramError opts (mkLocMessage MCFatal span msg)

throwProgramError :: LinkDepsOpts -> SDoc -> IO a
throwProgramError opts doc = throwGhcExceptionIO (ProgramError (renderWithContext (ldPprOpts opts) doc))

checkNonStdWay :: LinkDepsOpts -> Interp -> SrcSpan -> IO (Maybe FilePath)
checkNonStdWay _opts interp _srcspan
  -- On some targets (e.g. wasm) the RTS linker only supports loading
  -- dynamic code, in which case we need to ensure the .dyn_o object
  -- is picked (instead of .o which is also present because of
  -- -dynamic-too)
  | ldForceDyn _opts = do
      let target_ways = fullWays $ ldWays _opts
      pure $ if target_ways `hasWay` WayDyn
        then Nothing
        else Just $ waysTag (WayDyn `addWay` target_ways) ++ "_o"

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
