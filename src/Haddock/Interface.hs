-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Interface
-- Copyright   :  (c) Simon Marlow 2003-2006,
--                    David Waern  2006-2010
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
  createInterfaces
) where


import Haddock.GhcUtils
import Haddock.InterfaceFile
import Haddock.Interface.Create
import Haddock.Interface.AttachInstances
import Haddock.Interface.Rename
import Haddock.Options
import Haddock.Types
import Haddock.Utils

import Control.Monad
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Distribution.Verbosity
import System.Directory
import System.FilePath

import Digraph
import Exception
import GHC hiding (verbosity, flags)
import HscTypes


-- | Create 'Interface' structures by typechecking the list of modules
-- using the GHC API and processing the resulting syntax trees.
createInterfaces
  :: Verbosity -- ^ Verbosity of logging to 'stdout'
  -> [String] -- ^ A list of file or module names sorted by module topology
  -> [Flag] -- ^ Command-line flags
  -> [InterfaceFile] -- ^ Interface files of package dependencies
  -> Ghc ([Interface], LinkEnv)
  -- ^ Resulting list of interfaces and renaming environment
createInterfaces verbosity modules flags extIfaces = do

  out verbosity verbose "Creating interfaces..."
  let instIfaceMap =  Map.fromList [ (instMod iface, iface) | ext <- extIfaces
                                   , iface <- ifInstalledIfaces ext ]
  interfaces <- createInterfaces' verbosity modules flags instIfaceMap

  out verbosity verbose "Attaching instances..."
  interfaces' <- attachInstances interfaces instIfaceMap

  out verbosity verbose "Building link environment..."
  -- Combine the link envs of the external packages into one
  let extLinks  = Map.unions (map ifLinkEnv extIfaces)
      homeLinks = buildHomeLinks interfaces -- Build the environment for the home
                                            -- package
      links     = homeLinks `Map.union` extLinks
 
  out verbosity verbose "Renaming interfaces..."
  let warnings = Flag_NoWarnings `notElem` flags
  let (interfaces'', msgs) =
         runWriter $ mapM (renameInterface links warnings) interfaces'
  liftIO $ mapM_ putStrLn msgs

  return (interfaces'', homeLinks)


createInterfaces' :: Verbosity -> [String] -> [Flag] -> InstIfaceMap -> Ghc [Interface]
createInterfaces' verbosity modules flags instIfaceMap = do
  let useTempDir = Flag_NoTmpCompDir `notElem` flags

  -- Output dir needs to be set before calling depanal since it uses it to
  -- compute output file names that are stored in the DynFlags of the
  -- resulting ModSummaries.
  tmp <- liftIO getTemporaryDirectory
  x   <- liftIO getProcessID
  let tempDir = tmp </> ".haddock-" ++ show x
  when useTempDir $ modifySessionDynFlags (setOutputDir tempDir)

  targets <- mapM (\f -> guessTarget f Nothing) modules
  setTargets targets
  -- Dependency analysis.
  modgraph <- depanal [] False

  -- If template haskell is used by the package, we can't use HscNothing as
  -- target since we might need to run code generated from one or more of the
  -- modules during typechecking.
  if needsTemplateHaskell modgraph
    then
      -- Create a temporary directory in wich to write compilation output,
      -- unless the user has asked us not to.
      (if useTempDir then withTempDir tempDir else id) $ do
        -- Turn on compilation.
        let enableComp d = d { hscTarget = defaultObjectTarget }
        modifySessionDynFlags enableComp
        -- We need to update the DynFlags of the ModSummaries as well.
        let upd m = m { ms_hspp_opts = enableComp (ms_hspp_opts m) }
        let modgraph' = map upd modgraph

        processModules verbosity flags instIfaceMap modgraph'
    else
      processModules verbosity flags instIfaceMap modgraph


withTempDir :: (ExceptionMonad m, MonadIO m) => FilePath -> m a -> m a
withTempDir dir = gbracket_ (liftIO $ createDirectory dir)
                            (liftIO $ removeDirectoryRecursive dir)


processModules :: Verbosity -> [Flag] -> InstIfaceMap -> [ModSummary]
               -> Ghc [Interface]
processModules verbosity flags instIfaceMap mods = do
  let sortedMods = flattenSCCs $ topSortModuleGraph False mods Nothing
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
  tc_mod <- loadModule =<< typecheckModule =<< parseModule modsum
  if not $ isBootSummary modsum
    then do
      let filename = msHsFilePath modsum
      let dynflags = ms_hspp_opts modsum
      let Just renamed_src = renamedSource tc_mod
      let ghcMod = mkGhcModule (ms_mod modsum,
                            filename,
                            (parsedSource tc_mod,
                             renamed_src,
                             typecheckedSource tc_mod,
                             moduleInfo tc_mod))
                            dynflags
      out verbosity verbose "Creating interface..."
      (interface, msg) <- runWriterGhc $ createInterface ghcMod flags modMap instIfaceMap
      liftIO $ mapM_ putStrLn msg
      interface' <- liftIO $ evaluate interface
      return (Just interface')
    else
      return Nothing


type CheckedMod = (Module, FilePath, FullyCheckedMod)


type FullyCheckedMod = (ParsedSource,
                        RenamedSource,
                        TypecheckedSource,
                        ModuleInfo)


-- | Dig out what we want from the typechecker output
mkGhcModule :: CheckedMod -> DynFlags -> GhcModule
mkGhcModule (mdl, file, checkedMod) dynflags = GhcModule {
  ghcModule         = mdl,
  ghcFilename       = file,
  ghcMbDocOpts      = mbOpts,
  ghcMbDocHdr       = mbDocHdr,
  ghcGroup          = group_,
  ghcMbExports      = mbExports,
  ghcExportedNames  = modInfoExports modInfo,
  ghcDefinedNames   = map getName $ modInfoTyThings modInfo,
  ghcNamesInScope   = fromJust $ modInfoTopLevelScope modInfo,
  ghcInstances      = modInfoInstances modInfo,
  ghcDynFlags       = dynflags
}
  where
    mbOpts = haddockOptions dynflags
    (group_, _, mbExports, mbDocHdr) = renamed
    (_, renamed, _, modInfo) = checkedMod


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
