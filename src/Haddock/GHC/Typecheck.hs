--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.GHC.Typecheck (
  typecheckFiles  
) where


import Haddock.Exception
import Haddock.Utils.GHC
import Haddock.Types

import Data.Maybe
import Control.Monad
import GHC
import Digraph
import BasicTypes
import SrcLoc


typecheckFiles :: Session -> [FilePath] -> IO [GhcModule]
typecheckFiles session files = do
  checkedMods <- sortAndCheckModules session files
  return (map mkGhcModule checkedMods)


-- | Get the sorted graph of all loaded modules and their dependencies
getSortedModuleGraph :: Session -> IO [(Module, FilePath)]
getSortedModuleGraph session = do
  mbModGraph <- depanal session [] True
  moduleGraph <- case mbModGraph of
    Just mg -> return mg
    Nothing -> throwE "Failed to load all modules"
  let
    getModFile    = fromJust . ml_hs_file . ms_location
    sortedGraph   = topSortModuleGraph False moduleGraph Nothing
    sortedModules = concatMap flattenSCC sortedGraph
    modsAndFiles  = [ (ms_mod modsum, getModFile modsum) |
                      modsum <- sortedModules ]
  return modsAndFiles


type CheckedMod = (Module, FilePath, FullyCheckedMod)


type FullyCheckedMod = (ParsedSource, 
                        RenamedSource, 
                        TypecheckedSource, 
                        ModuleInfo)


-- TODO: make it handle cleanup
sortAndCheckModules :: Session -> [FilePath] -> IO [CheckedMod]
sortAndCheckModules session files = do 

  -- load all argument files

  targets <- mapM (\f -> guessTarget f Nothing) files
  setTargets session targets 

  -- compute the dependencies and load them as well

  allMods <- getSortedModuleGraph session
  targets' <- mapM (\(_, f) -> guessTarget f Nothing) allMods
  setTargets session targets'

  flag <- load session LoadAllTargets
  when (failed flag) $ 
    throwE "Failed to load all needed modules"

  -- typecheck the argument modules

  let argMods = filter ((`elem` files) . snd) allMods

  checkedMods <- forM argMods $ \(mod, file) -> do
    mbMod <- checkModule session (moduleName mod) False
    case mbMod of
      Just (CheckedModule a (Just b) (Just c) (Just d) _) 
        -> return (mod, file, (a,b,c,d))
      _ -> throwE ("Failed to check module: " ++ moduleString mod)

  return checkedMods


-- | Dig out what we want from the typechecker output
mkGhcModule :: CheckedMod -> GhcModule 
mkGhcModule (mod, file, checkedMod) = GhcModule {
  ghcModule         = mod,
  ghcFilename       = file,
  ghcMbDocOpts      = mbOpts,
  ghcHaddockModInfo = info,
  ghcMbDoc          = mbDoc,
  ghcGroup          = group,
  ghcMbExports      = mbExports,
  ghcExportedNames  = modInfoExports modInfo,
  ghcNamesInScope   = fromJust $ modInfoTopLevelScope modInfo, 
  ghcInstances      = modInfoInstances modInfo
}
  where
    HsModule _ _ _ _ _ mbOpts _ _      = unLoc parsed
    (group, _, mbExports, mbDoc, info) = renamed
    (parsed, renamed, _, modInfo)      = checkedMod
