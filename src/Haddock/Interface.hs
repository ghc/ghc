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


import Haddock.GhcUtils
import Haddock.InterfaceFile
import Haddock.Interface.Create
import Haddock.Interface.AttachInstances
import Haddock.Interface.Rename
import Haddock.Options hiding (verbosity)
import Haddock.Types
import Haddock.Utils

import Control.Monad
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Distribution.Verbosity
import System.Directory
import System.FilePath
import Text.Printf

import Digraph
import DynFlags hiding (verbosity)
import Exception
import GHC hiding (verbosity)
import HscTypes
import FastString (unpackFS)

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

  out verbosity verbose "Creating interfaces..."
  let instIfaceMap =  Map.fromList [ (instMod iface, iface) | ext <- extIfaces
                                   , iface <- ifInstalledIfaces ext ]
  interfaces <- createIfaces0 verbosity modules flags instIfaceMap

  let exportedNames =
        Set.unions $ map (Set.fromList . ifaceExports) $
        filter (\i -> not $ OptHide `elem` ifaceOptions i) interfaces
      mods = Set.fromList $ map ifaceMod interfaces
  out verbosity verbose "Attaching instances..."
  interfaces' <- attachInstances (exportedNames, mods) interfaces instIfaceMap

  out verbosity verbose "Building cross-linking environment..."
  -- Combine the link envs of the external packages into one
  let extLinks  = Map.unions (map ifLinkEnv extIfaces)
      homeLinks = buildHomeLinks interfaces -- Build the environment for the home
                                            -- package
      links     = homeLinks `Map.union` extLinks

  out verbosity verbose "Renaming interfaces..."
  let warnings = Flag_NoWarnings `notElem` flags
  dflags <- getDynFlags
  let (interfaces'', msgs) =
         runWriter $ mapM (renameInterface dflags links warnings) interfaces'
  liftIO $ mapM_ putStrLn msgs

  return (interfaces'', homeLinks)


--------------------------------------------------------------------------------
-- * Module typechecking and Interface creation
--------------------------------------------------------------------------------


createIfaces0 :: Verbosity -> [String] -> [Flag] -> InstIfaceMap -> Ghc [Interface]
createIfaces0 verbosity modules flags instIfaceMap =
  -- Output dir needs to be set before calling depanal since depanal uses it to
  -- compute output file names that are stored in the DynFlags of the
  -- resulting ModSummaries.
  (if useTempDir then withTempOutputDir else id) $ do
    modGraph <- depAnalysis
    if needsTemplateHaskell modGraph then do
      modGraph' <- enableCompilation modGraph
      createIfaces verbosity flags instIfaceMap modGraph'
    else
      createIfaces verbosity flags instIfaceMap modGraph

  where
    useTempDir :: Bool
    useTempDir = Flag_NoTmpCompDir `notElem` flags


    withTempOutputDir :: Ghc a -> Ghc a
    withTempOutputDir action = do
      tmp <- liftIO getTemporaryDirectory
      x   <- liftIO getProcessID
      let dir = tmp </> ".haddock-" ++ show x
      modifySessionDynFlags (setOutputDir dir)
      withTempDir dir action


    depAnalysis :: Ghc ModuleGraph
    depAnalysis = do
      targets <- mapM (\f -> guessTarget f Nothing) modules
      setTargets targets
      depanal [] False


    enableCompilation :: ModuleGraph -> Ghc ModuleGraph
    enableCompilation modGraph = do
      let enableComp d = let platform = targetPlatform d
                         in d { hscTarget = defaultObjectTarget platform }
      modifySessionDynFlags enableComp
      -- We need to update the DynFlags of the ModSummaries as well.
      let upd m = m { ms_hspp_opts = enableComp (ms_hspp_opts m) }
      let modGraph' = map upd modGraph
      return modGraph'


createIfaces :: Verbosity -> [Flag] -> InstIfaceMap -> ModuleGraph -> Ghc [Interface]
createIfaces verbosity flags instIfaceMap mods = do
  let sortedMods = flattenSCCs $ topSortModuleGraph False mods Nothing
  out verbosity normal "Haddock coverage:"
  (ifaces, _) <- foldM f ([], Map.empty) sortedMods
  return (reverse ifaces)
  where
    f (ifaces, ifaceMap) modSummary = do
      x <- processModule verbosity modSummary flags ifaceMap instIfaceMap
      return $ case x of
        Just iface -> (iface:ifaces, Map.insert (ifaceMod iface) iface ifaceMap)
        Nothing    -> (ifaces, ifaceMap) -- Boot modules don't generate ifaces.


processModule :: Verbosity -> ModSummary -> [Flag] -> IfaceMap -> InstIfaceMap -> Ghc (Maybe Interface)
processModule verbosity modsum flags modMap instIfaceMap = do
  out verbosity verbose $ "Checking module " ++ moduleString (ms_mod modsum) ++ "..."
  tm <- loadModule =<< typecheckModule =<< parseModule modsum
  if not $ isBootSummary modsum then do
    out verbosity verbose "Creating interface..."
    (interface, msg) <- runWriterGhc $ createInterface tm flags modMap instIfaceMap
    liftIO $ mapM_ putStrLn msg
    dflags <- getDynFlags
    let (haddockable, haddocked) = ifaceHaddockCoverage interface
        percentage = round (fromIntegral haddocked * 100 / fromIntegral haddockable :: Double) :: Int
        modString = moduleString (ifaceMod interface)
        coverageMsg = printf " %3d%% (%3d /%3d) in '%s'" percentage haddocked haddockable modString
        header = case ifaceDoc interface of
          Documentation Nothing _ -> False
          _ -> True
        undocumentedExports = [ formatName s n | ExportDecl { expItemDecl = L s n
                                                            , expItemMbDoc = (Documentation Nothing _, _)
                                                            } <- ifaceExportItems interface ]
          where
            formatName :: SrcSpan -> HsDecl Name -> String
            formatName loc n = p (getMainDeclBinder n) ++ case loc of
              RealSrcSpan rss -> " (" ++ unpackFS (srcSpanFile rss) ++ ":" ++ show (srcSpanStartLine rss) ++ ")"
              _ -> ""

            p [] = ""
            p (x:_) = let n = pretty dflags x
                          ms = modString ++ "."
                      in if ms `isPrefixOf` n
                         then drop (length ms) n
                         else n

    out verbosity normal coverageMsg
    when (Flag_PrintMissingDocs `elem` flags && (header || not (null undocumentedExports))) $ do
      out verbosity normal "  Missing documentation for:"
      unless header $ out verbosity normal "    Module header"
      mapM_ (out verbosity normal . ("    " ++)) undocumentedExports
    interface' <- liftIO $ evaluate interface
    return (Just interface')
  else
    return Nothing


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
buildHomeLinks ifaces = foldl upd Map.empty (reverse ifaces)
  where
    upd old_env iface
      | OptHide    `elem` ifaceOptions iface = old_env
      | OptNotHome `elem` ifaceOptions iface =
        foldl' keep_old old_env exported_names
      | otherwise = foldl' keep_new old_env exported_names
      where
        exported_names = ifaceVisibleExports iface
        mdl            = ifaceMod iface
        keep_old env n = Map.insertWith (\_ old -> old) n mdl env
        keep_new env n = Map.insert n mdl env


--------------------------------------------------------------------------------
-- * Utils
--------------------------------------------------------------------------------


withTempDir :: (ExceptionMonad m, MonadIO m) => FilePath -> m a -> m a
withTempDir dir = gbracket_ (liftIO $ createDirectory dir)
                            (liftIO $ removeDirectoryRecursive dir)
