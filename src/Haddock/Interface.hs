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
import Name
import Digraph
import HscTypes


-- | Turn a topologically sorted list of module names/filenames into interfaces. Also
-- return the home link environment created in the process.
#if __GLASGOW_HASKELL__ >= 609
createInterfaces :: Verbosity -> [String] -> [Flag] -> [InterfaceFile]
                 -> Ghc ([Interface], LinkEnv)
createInterfaces verbosity modules flags extIfaces = do
  -- part 1, create interfaces
  let instIfaceMap =  Map.fromList [ (instMod iface, iface) | ext <- extIfaces
                                   , iface <- ifInstalledIfaces ext ]
  out verbosity verbose "Creating interfaces..."
  interfaces <- createInterfaces' verbosity modules flags instIfaceMap
#else
createInterfaces :: Verbosity -> Session -> [String] -> [Flag]
                 -> [InterfaceFile] -> IO ([Interface], LinkEnv)
createInterfaces verbosity session modules flags extIfaces = do
  -- part 1, create interfaces
  let instIfaceMap =  Map.fromList [ (instMod iface, iface) | ext <- extIfaces
                                   , iface <- ifInstalledIfaces ext ]
  out verbosity verbose "Creating interfaces..."
  interfaces <- createInterfaces' verbosity session modules flags instIfaceMap
#endif
  -- part 2, build link environment
  out verbosity verbose "Building link environment..."
      -- combine the link envs of the external packages into one
  let extLinks  = Map.unions (map ifLinkEnv extIfaces)
      homeLinks = buildHomeLinks interfaces -- build the environment for the home
                                            -- package
      links     = homeLinks `Map.union` extLinks
      allNames  = Map.keys links

  -- part 3, attach instances
  out verbosity verbose "Attaching instances..."
  let interfaces' = attachInstances interfaces allNames

  -- part 4, rename interfaces
  out verbosity verbose "Renaming interfaces..."
  let warnings = Flag_NoWarnings `notElem` flags
  let (interfaces'', msgs) = 
         runWriter $ mapM (renameInterface links warnings) interfaces'
  liftIO $ mapM_ putStrLn msgs

  return (interfaces'', homeLinks)  


#if __GLASGOW_HASKELL__ >= 609
createInterfaces' :: Verbosity -> [String] -> [Flag] -> InstIfaceMap -> Ghc [Interface]
createInterfaces' verbosity modules flags instIfaceMap = do
  targets <- mapM (\f -> guessTarget f Nothing) modules
  setTargets targets
  modgraph <- depanal [] False

#if (__GLASGOW_HASKELL__ == 610 && __GHC_PATCHLEVEL__ >= 2) || __GLASGOW_HASKELL__ >= 611
  -- If template haskell is used by the package, we can not use
  -- HscNothing as target since we might need to run code generated from
  -- one or more of the modules during typechecking.
#if __GLASGOW_HASKELL__ < 611
  let needsTemplateHaskell = any (dopt Opt_TemplateHaskell . ms_hspp_opts)
#endif
  modgraph' <- if needsTemplateHaskell modgraph
       then do
         dflags <- getSessionDynFlags
         setSessionDynFlags dflags { hscTarget = HscC } 
         -- we need to set HscC on all the ModSummaries as well
         let addHscC m = m { ms_hspp_opts = (ms_hspp_opts m) { hscTarget = HscC } }  
         return (map addHscC modgraph)
       else return modgraph
#else
  let modgraph' = modgraph
#endif

  let orderedMods = flattenSCCs $ topSortModuleGraph False modgraph' Nothing
  (ifaces, _) <- foldM (\(ifaces, modMap) modsum -> do
    x <- processModule verbosity modsum flags modMap instIfaceMap
#else
createInterfaces' :: Verbosity -> Session -> [String] -> [Flag] -> InstIfaceMap -> IO [Interface]
createInterfaces' verbosity session modules flags instIfaceMap = do
  targets <- mapM (\f -> guessTarget f Nothing) modules
  setTargets session targets
  mbGraph <- depanal session [] False
  modgraph <- case mbGraph of
    Just graph -> return graph
    Nothing -> throwE "Failed to create dependency graph"
  let orderedMods = flattenSCCs $ topSortModuleGraph False modgraph Nothing
  (ifaces, _) <- foldM (\(ifaces, modMap) modsum -> do
    x <- processModule verbosity session modsum flags modMap instIfaceMap
#endif
    case x of
      Just interface ->
        return $ (interface : ifaces , Map.insert (ifaceMod interface) interface modMap)
      Nothing -> return (ifaces, modMap)
    ) ([], Map.empty) orderedMods
  return (reverse ifaces)

{-    liftIO $ do
     putStrLn . ppModInfo $ ifaceInfo interface
     putStrLn . show $ fmap pretty (ifaceDoc interface)
     print (ifaceOptions interface)
     mapM (putStrLn . pretty . fst) (Map.elems . ifaceDeclMap $ interface)
     mapM (putStrLn . show . fmap pretty . snd) (Map.elems . ifaceDeclMap $ interface)
     mapM (putStrLn . ppExportItem) (ifaceExportItems interface)
     mapM (putStrLn . pretty) (ifaceLocals interface)
     mapM (putStrLn . pretty) (ifaceExports interface)
     mapM (putStrLn . pretty) (ifaceVisibleExports interface)
     mapM (putStrLn . pretty) (ifaceInstances interface)
     mapM (\(a,b) -> putStrLn $ pretty a ++ pretty b)  (Map.toList $ ifaceSubMap interface)
     mapM (putStrLn . pretty) (ifaceInstances interface)-}

{-

ppInsts = concatMap ppInst 

ppInst (a,b,c) = concatMap pretty a ++ pretty b ++ concatMap pretty c 


ppExportItem (ExportDecl decl (Just doc) insts) = pretty decl ++ pretty doc ++ ppInsts insts
ppExportItem (ExportDecl decl Nothing insts) = pretty decl ++ ppInsts insts
ppExportItem (ExportNoDecl name name2 names) = pretty name ++ pretty name2 ++ pretty names
ppExportItem (ExportGroup level id doc) = show level ++ show id ++ pretty doc
ppExportItem (ExportDoc doc) = pretty doc
ppExportItem (ExportModule mod) = pretty mod


ppModInfo (HaddockModInfo a b c d) = show (fmap pretty a) ++ show b ++ show c ++ show d 
-}

#if __GLASGOW_HASKELL__ >= 609
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
      let (interface, msg) = runWriter $ createInterface ghcMod flags modMap instIfaceMap
      liftIO $ mapM_ putStrLn msg
      liftIO $ evaluate interface
      return (Just interface)
    else
      return Nothing
#else
processModule :: Verbosity -> Session -> ModSummary -> [Flag] -> ModuleMap -> InstIfaceMap -> IO (Maybe Interface)
processModule verbosity session modsum flags modMap instIfaceMap = do
  out verbosity verbose $ "Checking module " ++ moduleString (ms_mod modsum) ++ "..."
  let filename = msHsFilePath modsum
  mbMod <- checkAndLoadModule session modsum False
  if not $ isBootSummary modsum
    then do
      ghcMod <- case mbMod of
        Just (CheckedModule a (Just b) (Just c) (Just d) _)
          -> return $ mkGhcModule (ms_mod modsum, filename, (a,b,c,d)) (ms_hspp_opts modsum)
        _ -> throwE ("Failed to check module: " ++ (moduleString $ ms_mod modsum))
      let (interface, msg) = runWriter $ createInterface ghcMod flags modMap instIfaceMap
      mapM_ putStrLn msg
      return (Just interface)
    else
      return Nothing
#endif


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
  ghcHaddockModInfo = info,
  ghcMbDoc          = mbDoc,
  ghcGroup          = group_,
  ghcMbExports      = mbExports,
  ghcExportedNames  = modInfoExports modInfo,
  ghcDefinedNames   = map getName $ modInfoTyThings modInfo,
  ghcNamesInScope   = fromJust $ modInfoTopLevelScope modInfo, 
  ghcInstances      = modInfoInstances modInfo
}
  where
#if __GLASGOW_HASKELL__ == 608 && __GHC_PATCHLEVEL__ == 2
    HsModule _ _ _ _ _ mbOpts _ _ = unLoc parsed
#else
    mbOpts = haddockOptions dynflags
#endif
    (group_, _, mbExports, mbDoc, info) = renamed
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
