--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.GHC.Typecheck (
  typecheckFiles  
) where


import Haddock.Exception
import Haddock.Types
import Haddock.GHC.Utils

import Data.Maybe
import Control.Monad
import GHC
import Digraph
import BasicTypes
import SrcLoc

import Data.List


type CheckedMod = (Module, FilePath, FullyCheckedMod)


type FullyCheckedMod = (ParsedSource, 
                        RenamedSource, 
                        TypecheckedSource, 
                        ModuleInfo)


-- TODO: make it handle cleanup
typecheckFiles :: Session -> [FilePath] -> IO [GhcModule]
typecheckFiles session files = do 

  -- load all argument files

  targets <- mapM (\f -> guessTarget f Nothing) files
  setTargets session targets

  flag <- load session LoadAllTargets
  when (failed flag) $ 
    throwE "Failed to load all needed modules"

  modgraph <- getModuleGraph session

  let mods = concatMap flattenSCC $ topSortModuleGraph False modgraph Nothing
      getModFile = fromJust . ml_hs_file . ms_location
      mods'= [ (ms_mod modsum, ms_hspp_opts modsum, getModFile modsum) |
               modsum <- mods ]

  -- typecheck the argument modules

  ghcMods <- forM mods' $ \(mod, flags, file) -> do
    mbMod <- checkModule session (moduleName mod) False
    case mbMod of
      Just (CheckedModule a (Just b) (Just c) (Just d) _) 
        -> return $ mkGhcModule (mod, file, (a,b,c,d)) flags
      _ -> throwE ("Failed to check module: " ++ moduleString mod)

  return ghcMods

-- | Dig out what we want from the typechecker output
mkGhcModule :: CheckedMod -> DynFlags -> GhcModule 
mkGhcModule (mod, file, checkedMod) dynflags = GhcModule {
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
