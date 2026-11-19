-- The transition from Int to Word64 for uniques makes functions slightly larger
-- without this GHC option some optimizations fail to fire.
-- See https://gitlab.haskell.org/ghc/ghc/-/merge_requests/10568#note_505751
{-# OPTIONS_GHC -fspec-constr-threshold=10000 #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.Linker.Deps
  ( LinkDepsOpts (..)
  , LinkDeps (..)
  , getLinkDeps
  , getLinkDepsIfAvailable
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
import GHC.Utils.Panic
import GHC.Utils.Error

import GHC.Unit.Env
import GHC.Unit.Finder
import GHC.Unit.Module
import GHC.Unit.Home.ModInfo

import GHC.Iface.Errors.Types

import GHC.Utils.Misc
import qualified GHC.Unit.Home.Graph as HUG
import GHC.Data.Maybe

import Control.Applicative
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT, throwE)

import Data.List (isSuffixOf)

import System.FilePath
import System.Directory

data LinkDepsOpts = LinkDepsOpts
  { ldObjSuffix   :: !String                        -- ^ Suffix of .o files
  , ldForceDyn    :: !Bool                          -- ^ Always use .dyn_o?
  , ldUnitEnv     :: !UnitEnv
  , ldPprOpts     :: !SDocContext                   -- ^ Rendering options for error messages
  , ldUseByteCode :: !Bool                          -- ^ Use bytecode rather than objects
  , ldMsgOpts     :: !(DiagnosticOpts IfaceMessage) -- ^ Options for diagnostics
  , ldWays        :: !Ways                          -- ^ Enabled ways
  , ldFinderCache :: !FinderCache
  , ldFinderOpts  :: !FinderOpts
  , ldFindByteCode :: !(Module -> ModLocation -> IO (Maybe (IO Linkable)))
  , ldGetDependencies :: !([Module] -> IO ([Module], UniqDSet UnitId))
  }

data LinkDeps = LinkDeps
  { ldNeededLinkables :: [Linkable]
  , ldAllLinkables    :: [LinkableUsage]
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
      result <- planLinkDeps False opts interp pls span mods
      LinkDepsPlan loadable loaded units all_units <- either (dieWith opts span) pure result
      lnks <- sequence loadable
      let usages = mkLinkablesUsage lnks
      -- Do not retain 'Linkable' through lazy usage records.
      pure $! seqList usages (LinkDeps
        { ldNeededLinkables = lnks
        , ldAllLinkables = loaded ++ usages
        , ldUnits = units
        , ldNeededUnits = all_units
        })

-- | Find the required packages if all home artifacts are available.
-- Invalid interfaces and invalid ways still report errors.
getLinkDepsIfAvailable :: LinkDepsOpts -> Interp -> LoaderState -> SrcSpan
                       -> [Module] -> IO (Maybe [UnitId])
getLinkDepsIfAvailable opts interp pls span mods = do
  result <- planLinkDeps True opts interp pls span mods
  pure $ case result of
    Right (LinkDepsPlan _ _ units _) -> Just units
    Left _ -> Nothing

-- | Newly needed linkables may still need bytecode materialization.
data LinkDepsPlan = LinkDepsPlan [IO Linkable] [LinkableUsage] [UnitId] (UniqDSet UnitId)

planLinkDeps :: Bool -> LinkDepsOpts -> Interp -> LoaderState -> SrcSpan
             -> [Module] -> IO (Either SDoc LinkDepsPlan)
planLinkDeps check_artifacts opts interp pls span mods = do
  suffix <- checkNonStdWay opts interp span
  get_link_deps check_artifacts opts pls suffix mods

get_link_deps
  :: Bool
  -> LinkDepsOpts
  -> LoaderState
  -> Maybe FilePath  -- replace object suffixes?
  -> [Module]
  -> IO (Either SDoc LinkDepsPlan)
get_link_deps check_artifacts opts pls maybe_normal_osuf mods = runExceptT $ do

      -- Three step process:

        -- 1. Find the dependent home-pkg-modules/packages from each iface
        -- (omitting modules from the interactive package, which is already linked)
      (mods_s, pkgs_s) <- lift (ldGetDependencies opts relevant_mods)

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
      loadable <- mapM (get_linkable (ldObjSuffix opts)) mods_needed
      pure (LinkDepsPlan loadable links_got pkgs_needed pkgs_s)
  where
    unit_env = ldUnitEnv opts
    relevant_mods = filterOut isInteractiveModule mods

    no_obj mod = missing (text "cannot find object file for module" <+> quotes (ppr mod))
    missing reason = throwE (reason $$ text "while linking an interpreted expression")

    get_linkable osuf mod = do
      info <- lift $ HUG.lookupHugByModule mod (ue_home_unit_graph unit_env)
      case info of
        Just hmi
          | Just lnk <- preferredHomeLinkable opts hmi -> checked_linkable osuf lnk
          | otherwise -> no_obj mod
        Nothing
          | Just home_unit <- ue_homeUnit unit_env -> do
              -- In one-shot mode, prefer bytecode. Discovery returns an
              -- action so a probe never compiles Core or writes foreign stubs.
              found <- lift $ runFinderM $ findHomeModule
                         (ldFinderCache opts) (ldFinderOpts opts) home_unit (moduleName mod)
              case found of
                Found loc _ -> do
                  bytecode <- lift $ ldFindByteCode opts mod loc
                  case bytecode of
                    Just load -> pure load
                    Nothing -> do
                      object <- lift $ findObjectLinkableMaybe mod loc
                      maybe (no_obj mod) (checked_linkable osuf) object
                _ -> no_obj mod
        Nothing -> no_obj mod

    checked_linkable osuf lnk = do
      parts <- traverse (adjust_part osuf) (linkableParts lnk)
      pure (pure (lnk {linkableParts = parts}))

    adjust_part osuf part = case (maybe_normal_osuf, part) of
      (Just new_osuf, DotO file ModuleObject) -> do
        massert (osuf `isSuffixOf` file)
        let file_base = fromJust (stripExtension osuf file)
            new_file = file_base <.> new_osuf
        requireFile new_file
        pure (DotO new_file ModuleObject)
      (Just _, DotA fp) -> panic ("adjust_ul DotA " ++ show fp)
      (Just _, DotDLL fp) -> panic ("adjust_ul DotDLL " ++ show fp)
      (_, DotO file _) | check_artifacts -> requireFile file >> pure part
      (_, DotA file) | check_artifacts -> requireFile file >> pure part
      (_, DotDLL file) | check_artifacts -> requireFile file >> pure part
      -- Inspecting DotGBC payloads here can force lazy code generation from
      -- the home table. Serialized bytecode carries its foreign object bytes.
      _ -> pure part

    requireFile file = do
      exists <- lift (doesFileExist file)
      unless exists (missing (text "cannot find object file" <+> quotes (text file)))

-- | Select the same in-memory linkable for planning and linking.
preferredHomeLinkable :: LinkDepsOpts -> HomeModInfo -> Maybe Linkable
preferredHomeLinkable opts hmi
  | ldUseByteCode opts = homeModInfoByteCode hmi <|> homeModInfoObject hmi
  | otherwise = homeModInfoObject hmi <|> homeModInfoByteCode hmi

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
