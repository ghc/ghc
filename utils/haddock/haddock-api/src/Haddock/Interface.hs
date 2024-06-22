{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
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


import Haddock.GhcUtils (moduleString, pretty)
import Haddock.Interface.AttachInstances (attachInstances)
import Haddock.Interface.Create (createInterface1)
import Haddock.Interface.Rename (renameInterface)
import Haddock.InterfaceFile (InterfaceFile, ifInstalledIfaces, ifLinkEnv)
import Haddock.Options hiding (verbosity)
import Haddock.Types
import Haddock.Utils (Verbosity (..), normal, out, verbose)

import Control.Monad
import Data.List (isPrefixOf)
import Data.Traversable (for)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace (traceMarkerIO)
import System.Exit (exitFailure ) -- TODO use Haddock's die
import Text.Printf

import GHC hiding (verbosity, SuccessFlag(..))
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
import GHC.HsToCore.Docs (getMainDeclBinder)
import GHC.Types.Error (mkUnknownDiagnostic)
import GHC.Types.Name.Occurrence (emptyOccEnv)
import GHC.Unit.Module.Graph (ModuleGraphNode (..))
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.ModSummary (isBootSummary)
import GHC.Utils.Outputable (Outputable, (<+>), pprModuleName)
import GHC.Utils.Error (withTiming)
import GHC.Unit.Home.ModInfo
import GHC.Tc.Utils.Env (lookupGlobal_maybe)
import qualified Data.List as List

#if defined(mingw32_HOST_OS)
import System.IO
import GHC.IO.Encoding.CodePage (mkLocaleEncoding)
import GHC.IO.Encoding.Failure (CodingFailureMode(TransliterateCodingFailure))
#endif

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
#if defined(mingw32_HOST_OS)
  -- Avoid internal error: <stderr>: hPutChar: invalid argument (invalid character)' non UTF-8 Windows
  liftIO $ hSetEncoding stdout $ mkLocaleEncoding TransliterateCodingFailure
  liftIO $ hSetEncoding stderr $ mkLocaleEncoding TransliterateCodingFailure
#endif

  dflags <- getDynFlags

  -- Map from a module to a corresponding installed interface
  let instIfaceMap :: InstIfaceMap
      instIfaceMap = Map.fromList
        [ (instMod iface, iface)
        | ext <- extIfaces
        , iface <- ifInstalledIfaces ext
        ]

  interfaces <- createIfaces verbosity modules flags instIfaceMap

  let exportedNames =
        Set.unions $ map (Set.fromList . ifaceExports) $
        filter (\i -> not $ OptHide `elem` ifaceOptions i) interfaces
      mods = Set.fromList $ map ifaceMod interfaces

  interfaces' <- {-# SCC attachInstances #-}
                 withTimingM "attachInstances" (const ()) $ do
                   attachInstances (exportedNames, mods) interfaces instIfaceMap

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
  targets <- mapM (\filePath -> guessTarget filePath Nothing Nothing) modules
  setTargets targets
  (_errs, modGraph) <- depanalE [] False

  liftIO $ traceMarkerIO "Load started"
  -- Create (if necessary) and load .hi-files.
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
  (ifaces, _) <- foldM f ([], Map.empty) sortedMods
  return (reverse ifaces)
  where
    f (ifaces, ifaceMap) modSummary = do
      x <- {-# SCC processModule #-}
           withTimingM "processModule" (const ()) $ do
             processModule verbosity modSummary flags ifaceMap instIfaceMap
      return $ case x of
        Just iface -> ( iface:ifaces
                      , Map.insert (ifaceMod iface) iface ifaceMap )
        Nothing    -> ( ifaces
                      , ifaceMap ) -- Boot modules don't generate ifaces.

dropErr :: MaybeErr e a -> Maybe a
dropErr (Succeeded a) = Just a
dropErr (Failed _) = Nothing

processModule :: Verbosity -> ModSummary -> [Flag] -> IfaceMap -> InstIfaceMap -> Ghc (Maybe Interface)
processModule verbosity modSummary flags ifaceMap instIfaceMap = do
  out verbosity verbose $ "Checking module " ++ moduleString (ms_mod modSummary) ++ "..."

  hsc_env <- getSession
  dflags <- getDynFlags
  let sDocContext = DynFlags.initSDocContext dflags Outputable.defaultUserStyle
  let hmi = case lookupHpt (hsc_HPT hsc_env) (moduleName $ ms_mod modSummary) of
        Nothing -> error "processModule: All modules should be loaded into the HPT by this point"
        Just x -> x
      mod_iface = hm_iface hmi
      unit_state = hsc_units hsc_env

      cls_insts = instEnvElts . md_insts $ hm_details hmi

      fam_insts = md_fam_insts $ hm_details hmi

      insts = (cls_insts, fam_insts)

  !interface <- do
    logger <- getLogger
    {-# SCC createInterface #-}
      withTiming logger "createInterface" (const ()) $
        runIfM (liftIO . fmap dropErr . lookupGlobal_maybe hsc_env) $
          createInterface1 flags unit_state modSummary mod_iface ifaceMap instIfaceMap insts

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
