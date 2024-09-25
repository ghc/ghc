{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE TupleSections     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Interface
-- Copyright   :  (c) Simon Marlow      2003-2006,
--                    David Waern       2006-2010,
--                    Mateusz Kowalczyk 2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This module typechecks Haskell modules using the GHC API and processes
-- the result to create 'Interface's. The typechecking and the 'Interface'
-- creation is interleaved, so that when a module is processed, the
-- 'Interface's of all previously processed modules are available. The
-- creation of an 'Interface' from a typechecked module is delegated to
-- "Haddock.Interface.Create".
--
-- When all modules have been typechecked and processed, information about
-- instances are attached to each 'Interface'. This task is delegated to
-- "Haddock.Interface.AttachInstances". Note that this is done as a separate
-- step because GHC can't know about all instances until all modules have been
-- typechecked.
--
-- As a last step a link environment is built which maps names to the \"best\"
-- places to link to in the documentation, and all 'Interface's are \"renamed\"
-- using this environment.
-----------------------------------------------------------------------------
module Haddock.Interface (
  processModules
) where


import Control.Monad
import Data.List (isPrefixOf)
import qualified Data.List as List
import Data.Traversable (for)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace (traceMarkerIO)
import System.Exit (exitFailure ) -- TODO use Haddock's die
import Text.Printf
import GHC hiding (verbosity, SuccessFlag(..))
import GHC.Builtin.Names (mkMainModule_)
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (unpackFS)
import GHC.Data.Graph.Directed
import GHC.Data.Maybe
import GHC.Driver.Env
import GHC.Driver.Monad
import GHC.Driver.Make
import GHC.Driver.Main
import GHC.Core.InstEnv
import qualified GHC.Driver.DynFlags as DynFlags
import qualified GHC.Utils.Outputable as Outputable
import GHC.Driver.Session hiding (verbosity)
import GHC.Driver.Phases
import GHC.Driver.Pipeline (compileFile)
import GHC.HsToCore.Docs (getMainDeclBinder)
import GHC.Iface.Load (loadSysInterface)
import GHC.IfaceToCore (tcIfaceInst, tcIfaceFamInst)
import GHC.Tc.Utils.Monad (initIfaceLoad, initIfaceLcl)
import GHC.Tc.Utils.Env (lookupGlobal_maybe)
import GHC.Types.Error (mkUnknownDiagnostic)
import GHC.Types.Name.Occurrence (emptyOccEnv)
import GHC.Unit.Finder (findImportedModule, FindResult(Found))
import GHC.Unit.Home.ModInfo
import GHC.Unit.Module.Graph (ModuleGraphNode (..))
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.ModIface (mi_semantic_module, mi_boot)
import GHC.Unit.Module.ModSummary (isBootSummary)
import GHC.Utils.Outputable (Outputable, (<+>), pprModuleName, text)
import GHC.Utils.Error (withTiming)
import GHC.Utils.Monad (mapMaybeM)

import Haddock.GhcUtils (moduleString, pretty)
import Haddock.Interface.AttachInstances (attachInstances)
import Haddock.Interface.Create (createInterface1, createInterface1')
import Haddock.Interface.Rename (renameInterface)
import Haddock.InterfaceFile (InterfaceFile, ifInstalledIfaces, ifLinkEnv)
import Haddock.Options hiding (verbosity)
import Haddock.Types
import Haddock.Utils (Verbosity (..), normal, out, verbose)
import qualified Haddock.Compat as Compat

-- | Create 'Interface's and a link environment by typechecking the list of
-- modules using the GHC API and processing the resulting syntax trees.
processModules
  :: Verbosity                  -- ^ Verbosity of logging to 'stdout'
  -> [String]                   -- ^ A list of file or module names sorted by
                                -- module topology
  -> [Flag]                     -- ^ Command-line flags
  -> [InterfaceFile]            -- ^ Interface files of package dependencies
  -> Ghc ([Interface], LinkEnv) -- ^ Resulting list of interfaces and renaming
                                -- environment
processModules verbosity modules flags extIfaces = do
  liftIO Compat.setEncoding
  dflags <- getDynFlags

  -- Map from a module to a corresponding installed interface
  let instIfaceMap :: InstIfaceMap
      instIfaceMap = Map.fromList
        [ (instMod iface, iface)
        | ext <- extIfaces
        , iface <- ifInstalledIfaces ext
        ]
      oneShotHiFile = optOneShot flags

  interfaces <- maybe
    (createIfaces verbosity modules flags instIfaceMap)
    (createOneShotIface verbosity flags instIfaceMap)
    oneShotHiFile

  let exportedNames =
        Set.unions $ map (Set.fromList . ifaceExports) $
        filter (\i -> not $ OptHide `elem` ifaceOptions i) interfaces
      mods = Set.fromList $ map ifaceMod interfaces

  interfaces' <- {-# SCC attachInstances #-}
                 withTimingM "attachInstances" (const ()) $ do
                   attachInstances (exportedNames, mods) interfaces instIfaceMap (isJust oneShotHiFile)

  -- Combine the link envs of the external packages into one
  let extLinks  = Map.unions (map ifLinkEnv extIfaces)
      homeLinks = buildHomeLinks interfaces' -- Build the environment for the home
                                             -- package
      links     = homeLinks `Map.union` extLinks

  let warnings = Flag_NoWarnings `notElem` flags
      ignoredSymbolSet = ignoredSymbols flags

  interfaces'' <-
    withTimingM "renameAllInterfaces" (const ()) $
      for interfaces' $ \i -> do
        withTimingM ("renameInterface: " <+> pprModuleName (moduleName (ifaceMod i))) (const ()) $
          renameInterface dflags ignoredSymbolSet links warnings (Flag_Hoogle `elem` flags) i

  return (interfaces'', homeLinks)

--------------------------------------------------------------------------------
-- * Module typechecking and Interface creation
--------------------------------------------------------------------------------

createIfaces
    :: Verbosity
    -- ^ Verbosity requested by the caller
    -> [String]
    -- ^ List of modules provided as arguments to Haddock (still in FilePath
    -- format)
    -> [Flag]
    -- ^ Command line flags which Hadddock was invoked with
    -> InstIfaceMap
    -- ^ Map from module to corresponding installed interface file
    -> Ghc [Interface]
    -- ^ Resulting interfaces
createIfaces verbosity modules flags instIfaceMap = do
  let (hs_srcs, non_hs_srcs) = List.partition isHaskellishTarget $ map (,Nothing) modules
  hsc_env <- getSession
  o_files <- mapMaybeM (\x -> liftIO $ compileFile hsc_env NoStop x)
             non_hs_srcs
  dflags <- getSessionDynFlags
  let dflags' = dflags { ldInputs = map (FileOption "") o_files
                                    ++ ldInputs dflags }
      dflags'' = if Flag_NoCompilation `elem` flags then dflags' { ghcMode = OneShot } else dflags'
  _ <- setSessionDynFlags dflags''
  targets <- mapM (\(filePath, _) -> guessTarget filePath Nothing Nothing) hs_srcs
  setTargets targets
  (_errs, modGraph) <- depanalE [] False

  -- Create (if necessary) and load .hi-files. With --no-compilation this happens later.
  when (Flag_NoCompilation `notElem` flags) $ do
    liftIO $ traceMarkerIO "Load started"
    success <- withTimingM "load'" (const ()) $
                load' noIfaceCache LoadAllTargets mkUnknownDiagnostic (Just batchMsg) modGraph
    when (failed success) $ do
      out verbosity normal "load' failed"
      liftIO exitFailure
    liftIO $ traceMarkerIO "Load ended"

      -- We topologically sort the module graph including boot files,
      -- so it should be acylic (hopefully we failed much earlier if this is not the case)
      -- We then filter out boot modules from the resultant topological sort
      --
      -- We do it this way to make 'buildHomeLinks' a bit more stable
      -- 'buildHomeLinks' depends on the topological order of its input in order
      -- to construct its result. In particular, modules closer to the bottom of
      -- the dependency chain are to be prefered for link destinations.
      --
      -- If there are cycles in the graph, then this order is indeterminate
      -- (the nodes in the cycle can be ordered in any way).
      -- While 'topSortModuleGraph' does guarantee stability for equivalent
      -- module graphs, seemingly small changes in the ModuleGraph can have
      -- big impacts on the `LinkEnv` constructed.
      --
      -- For example, suppose
      --  G1 = A.hs -> B.hs -> C.hs (where '->' denotes an import).
      --
      -- Then suppose C.hs is changed to have a cyclic dependency on A
      --
      --  G2 = A.hs -> B.hs -> C.hs -> A.hs-boot
      --
      -- For G1, `C.hs` is preferred for link destinations. However, for G2,
      -- the topologically sorted order not taking into account boot files (so
      -- C -> A) is completely indeterminate.
      -- Using boot files to resolve cycles, we end up with the original order
      -- [C, B, A] (in decreasing order of preference for links)
      --
      -- This exact case came up in testing for the 'base' package, where there
      -- is a big module cycle involving 'Prelude' on windows, but the cycle doesn't
      -- include 'Prelude' on non-windows platforms. This lead to drastically different
      -- LinkEnv's (and failing haddockHtmlTests) across the platforms
      --
      -- In effect, for haddock users this behaviour (using boot files to eliminate cycles)
      -- means that {-# SOURCE #-} imports no longer count towards re-ordering
      -- the preference of modules for linking.
      --
      -- i.e. if module A imports B, then B is preferred over A,
      -- but if module A {-# SOURCE #-} imports B, then we can't say the same.
      --
  let
      go (AcyclicSCC (ModuleNode _ ms))
        | NotBoot <- isBootSummary ms = [ms]
        | otherwise = []
      go (AcyclicSCC _) = []
      go (CyclicSCC _) = error "haddock: module graph cyclic even with boot files"

      -- Visit modules in that order
      sortedMods = concatMap go $ topSortModuleGraph False modGraph Nothing
  out verbosity normal "Haddock coverage:"
  let inst_warning_map = Map.unions $ map instWarningMap (Map.elems instIfaceMap)
  (ifaces, _, _) <- foldM f ([], Map.empty, inst_warning_map) sortedMods
  return (reverse ifaces)
  where
    f (ifaces, ifaceMap, warningMap) modSummary = do
      x <- {-# SCC processModule #-}
           withTimingM "processModule" (const ()) $ do
             processModule verbosity modSummary flags ifaceMap instIfaceMap warningMap
      return $ case x of
        Just iface -> ( iface:ifaces
                      , Map.insert (ifaceMod iface) iface ifaceMap
                      , Map.union (ifaceWarningMap iface) warningMap)
        Nothing    -> ( ifaces
                      , ifaceMap
                      , warningMap ) -- Boot modules don't generate ifaces.

dropErr :: MaybeErr e a -> Maybe a
dropErr (Succeeded a) = Just a
dropErr (Failed _) = Nothing

loadHiFile :: HscEnv -> Outputable.SDoc -> Module -> IO (ModIface, ([ClsInst], [FamInst]))
loadHiFile hsc_env doc theModule = initIfaceLoad hsc_env $ do

  mod_iface <- loadSysInterface doc theModule

  insts <- initIfaceLcl (mi_semantic_module mod_iface) doc (mi_boot mod_iface) $ do

    new_eps_insts     <- mapM tcIfaceInst (mi_insts mod_iface)
    new_eps_fam_insts <- mapM tcIfaceFamInst (mi_fam_insts mod_iface)

    pure (new_eps_insts, new_eps_fam_insts)

  pure (mod_iface, insts)

processModule :: Verbosity -> ModSummary -> [Flag] -> IfaceMap -> InstIfaceMap -> WarningMap -> Ghc (Maybe Interface)
processModule verbosity modSummary flags ifaceMap instIfaceMap warningMap = do
  out verbosity verbose $ "Checking module " ++ moduleString (ms_mod modSummary) ++ "..."

  hsc_env <- getSession
  dflags <- getDynFlags
  let sDocContext = DynFlags.initSDocContext dflags Outputable.defaultUserStyle
      doc = text "processModule"
      unit_state = hsc_units hsc_env

  (mod_iface, insts) <- if Flag_NoCompilation `elem` flags
    then liftIO $ loadHiFile hsc_env doc $ ms_mod modSummary
    else
      let hmi = case lookupHpt (hsc_HPT hsc_env) (moduleName $ ms_mod modSummary) of
            Nothing -> error "processModule: All modules should be loaded into the HPT by this point"
            Just x -> x
          cls_insts = instEnvElts . md_insts $ hm_details hmi
          fam_insts = md_fam_insts $ hm_details hmi

      in pure (hm_iface hmi, (cls_insts, fam_insts))

  !interface <- do
    logger <- getLogger
    {-# SCC createInterface #-}
      withTiming logger "createInterface" (const ()) $
        runIfM (liftIO . fmap dropErr . lookupGlobal_maybe hsc_env) $
          createInterface1 flags unit_state modSummary mod_iface ifaceMap instIfaceMap insts warningMap

  let
    (haddockable, haddocked) =
      ifaceHaddockCoverage interface

    percentage :: Int
    percentage = div (haddocked * 100) haddockable

    modString :: String
    modString = moduleString (ifaceMod interface)

    coverageMsg :: String
    coverageMsg =
      printf " %3d%% (%3d /%3d) in '%s'" percentage haddocked haddockable modString

    header :: Bool
    header = case ifaceDoc interface of
      Documentation Nothing _ -> False
      _ -> True

    undocumentedExports :: [String]
    undocumentedExports =
      [ formatName (locA s) n
      | ExportDecl ExportD
          { expDDecl = L s n
          , expDMbDoc = (Documentation Nothing _, _)
          } <- ifaceExportItems interface
      ]
        where
          formatName :: SrcSpan -> HsDecl GhcRn -> String
          formatName loc n = p (getMainDeclBinder emptyOccEnv n) ++ case loc of
            RealSrcSpan rss _ -> " (" ++ unpackFS (srcSpanFile rss) ++ ":" ++
              show (srcSpanStartLine rss) ++ ")"
            _ -> ""

          p :: Outputable a => [a] -> String
          p [] = ""
          p (x:_) = let n = pretty sDocContext x
                        ms = modString ++ "."
                    in if ms `isPrefixOf` n
                       then drop (length ms) n
                       else n

  when (OptHide `notElem` ifaceOptions interface) $ do
    out verbosity normal coverageMsg
    when (Flag_NoPrintMissingDocs `notElem` flags
          && not (null undocumentedExports && header)) $ do
      out verbosity normal "  Missing documentation for:"
      unless header $ out verbosity normal "    Module header"
      mapM_ (out verbosity normal . ("    " ++)) undocumentedExports

  return (Just interface)


-- | Create a single interface from a single module in one-shot mode.
createOneShotIface
    :: Verbosity
    -- ^ Verbosity requested by the caller
    -> [Flag]
    -- ^ Command line flags which Hadddock was invoked with
    -> InstIfaceMap
    -- ^ Map from module to corresponding installed interface file
    -> String
    -- ^ Name of the module
    -> Ghc [Interface]
    -- ^ Resulting interfaces
createOneShotIface verbosity flags instIfaceMap moduleNameStr = do

  let moduleNm = mkModuleName moduleNameStr
      doc = text "createOneShotIface"

  out verbosity verbose $ "Checking interface " ++ moduleNameStr ++ "..."

  -- Turn on GHC's one-shot mode
  dflags <- (\df -> df{ ghcMode = OneShot }) <$> getDynFlags
  modifySession $ hscSetFlags dflags
  hsc_env <- getSession

  (iface, insts) <- liftIO $ loadHiFile hsc_env doc $ mkMainModule_ moduleNm

  -- Update the DynFlags with the extensions from the source file (as stored in the interface file)
  -- This is instead of ms_hspp_opts from ModSummary, which is not available in one-shot mode.
  let dflags' = case mi_docs iface of
                  Just docs -> setExtensions $ setLanguage dflags
                    where
                      setLanguage df = lang_set df (docs_language docs)
                      setExtensions df = List.foldl' xopt_set df $ EnumSet.toList (docs_extensions docs)
                  Nothing -> dflags

  -- We should find the module here, otherwise there would have been an error earlier.
  res <- liftIO $ findImportedModule hsc_env moduleNm NoPkgQual
  let hieFilePath = case res of
                      Found ml _ -> ml_hie_file ml
                      _ -> throwE "createOneShotIface: module not found"
  let inst_warning_map = Map.unions $ map instWarningMap (Map.elems instIfaceMap)
  !interface <- do
    logger <- getLogger
    {-# SCC createInterface #-}
      withTiming logger "createInterface" (const ()) $
        runIfM (liftIO . fmap dropErr . lookupGlobal_maybe hsc_env) $
          createInterface1' flags (hsc_units hsc_env) dflags' hieFilePath iface mempty instIfaceMap insts inst_warning_map

  pure [interface]

--------------------------------------------------------------------------------
-- * Building of cross-linking environment
--------------------------------------------------------------------------------


-- | Build a mapping which for each original name, points to the "best"
-- place to link to in the documentation.  For the definition of
-- "best", we use "the module nearest the bottom of the dependency
-- graph which exports this name", not including hidden modules.  When
-- there are multiple choices, we pick a random one.
--
-- The interfaces are passed in in topologically sorted order, but we start
-- by reversing the list so we can do a foldl.
buildHomeLinks :: [Interface] -> LinkEnv
buildHomeLinks ifaces = List.foldl' upd Map.empty (reverse ifaces)
  where
    upd old_env iface
      | OptHide `elem` ifaceOptions iface =
          old_env
      | OptNotHome `elem` ifaceOptions iface =
          List.foldl' keep_old old_env exported_names
      | otherwise =
          List.foldl' keep_new old_env exported_names
      where
        exported_names = ifaceVisibleExports iface ++ map getName (ifaceInstances iface)
        mdl            = ifaceMod iface
        keep_old env n = Map.insertWith (\_ old -> old) n mdl env
        keep_new env n = Map.insert n mdl env
