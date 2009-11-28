-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Interface
-- Copyright   :  (c) Simon Marlow 2003-2006,
--                    David Waern  2006-2009
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Here we build the actual module interfaces. By interface we mean the
-- information that is used to render a Haddock page for a module. Parts of
-- this information are also stored in the .haddock files.
-----------------------------------------------------------------------------

module Haddock.Interface (
  createInterfaces
) where


import Haddock.Interface.Create
import Haddock.Interface.AttachInstances
import Haddock.Interface.Rename
import Haddock.Types
import Haddock.Options
import Haddock.GhcUtils
import Haddock.Utils
import Haddock.InterfaceFile

import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Control.Monad
import Control.Exception ( evaluate )
import Distribution.Verbosity

import GHC hiding (verbosity, flags)
import Digraph
import HscTypes


-- | Turn a topologically sorted list of module names/filenames into interfaces. Also
-- return the home link environment created in the process.
createInterfaces :: Verbosity -> [String] -> [Flag] -> [InterfaceFile]
                 -> Ghc ([Interface], LinkEnv)
createInterfaces verbosity modules flags extIfaces = do
  -- part 1, create interfaces
  let instIfaceMap =  Map.fromList [ (instMod iface, iface) | ext <- extIfaces
                                   , iface <- ifInstalledIfaces ext ]
  out verbosity verbose "Creating interfaces..."
  interfaces <- createInterfaces' verbosity modules flags instIfaceMap

  -- part 2, build link environment
  out verbosity verbose "Building link environment..."
      -- combine the link envs of the external packages into one
  let extLinks  = Map.unions (map ifLinkEnv extIfaces)
      homeLinks = buildHomeLinks interfaces -- build the environment for the home
                                            -- package
      links     = homeLinks `Map.union` extLinks

  -- part 3, attach instances
  out verbosity verbose "Attaching instances..."
  interfaces' <- attachInstances interfaces instIfaceMap

  -- part 4, rename interfaces
  out verbosity verbose "Renaming interfaces..."
  let warnings = Flag_NoWarnings `notElem` flags
  let (interfaces'', msgs) =
         runWriter $ mapM (renameInterface links warnings) interfaces'
  liftIO $ mapM_ putStrLn msgs

  return (interfaces'', homeLinks)


createInterfaces' :: Verbosity -> [String] -> [Flag] -> InstIfaceMap -> Ghc [Interface]
createInterfaces' verbosity modules flags instIfaceMap = do
  targets <- mapM (\f -> guessTarget f Nothing) modules
  setTargets targets
  modgraph <- depanal [] False

  -- If template haskell is used by the package, we can not use
  -- HscNothing as target since we might need to run code generated from
  -- one or more of the modules during typechecking.
  modgraph' <- if needsTemplateHaskell modgraph
       then do
         dflags <- getSessionDynFlags
         _ <- setSessionDynFlags dflags { hscTarget = defaultObjectTarget }
         -- we need to set defaultObjectTarget on all the ModSummaries as well
         let addHscAsm m = m { ms_hspp_opts = (ms_hspp_opts m) { hscTarget = defaultObjectTarget } }
         return (map addHscAsm modgraph)
       else return modgraph

  let orderedMods = flattenSCCs $ topSortModuleGraph False modgraph' Nothing
  (ifaces, _) <- foldM (\(ifaces, modMap) modsum -> do
    x <- processModule verbosity modsum flags modMap instIfaceMap
    case x of
      Just interface ->
        return $ (interface : ifaces , Map.insert (ifaceMod interface) interface modMap)
      Nothing -> return (ifaces, modMap)
    ) ([], Map.empty) orderedMods
  return (reverse ifaces)


processModule :: Verbosity -> ModSummary -> [Flag] -> ModuleMap -> InstIfaceMap -> Ghc (Maybe Interface)
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
  ghcInstances      = modInfoInstances modInfo
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
