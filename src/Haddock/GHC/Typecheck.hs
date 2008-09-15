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
typecheckFiles :: [FilePath] -> Ghc [GhcModule]
typecheckFiles files = do

  -- load all argument files

  targets <- mapM (\f -> guessTarget f Nothing) files
  setTargets targets

  flag <- load LoadAllTargets
  when (failed flag) $ 
    throwE "Failed to load all needed modules"

  modgraph <- getModuleGraph

  let mods = concatMap flattenSCC $ topSortModuleGraph False modgraph Nothing
      getModFile = fromJust . ml_hs_file . ms_location
      mods'= [ (ms_mod modsum, ms_hspp_opts modsum, getModFile modsum) |
               modsum <- mods ]

  -- typecheck the argument modules

  ghcMods <- forM mods' $ \(mod, flags, file) ->
    handleSourceError
        (\err -> do
           printExceptionAndWarnings err
           throwE ("Failed to check module: " ++ moduleString mod)) $
      do tc_mod <- typecheckModule =<< parseModule (moduleName mod)
         let Just renamed_src = renamedSource tc_mod
         return $ mkGhcModule (mod, file, (parsedSource tc_mod,
                                           renamed_src,
                                           typecheckedSource tc_mod,
                                           moduleInfo tc_mod))
                              flags

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
    (group, _, mbExports, mbDoc, info) = renamed
    (parsed, renamed, _, modInfo) = checkedMod
