--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.GHC.Typecheck (
  typecheckFiles,
  mkGhcModule
) where


import Haddock.Exception
import Haddock.Types
import Haddock.GHC.Utils

import Data.Maybe
import Control.Monad
import GHC
import HscTypes ( msHsFilePath )
import Digraph
import BasicTypes
import SrcLoc
import MonadUtils ( liftIO )

import Data.List


type CheckedMod = (Module, FilePath, FullyCheckedMod)


type FullyCheckedMod = (ParsedSource, 
                        RenamedSource, 
                        TypecheckedSource, 
                        ModuleInfo)


-- TODO: make it handle cleanup
typecheckFiles :: [FilePath] -> Ghc [GhcModule]
typecheckFiles files = do
    targets <- mapM (\f -> guessTarget f Nothing) files
    setTargets targets
    modgraph <- depanal [] False
    let ordered_mods = flattenSCCs $ topSortModuleGraph False modgraph Nothing
    process_mods ordered_mods
  where
    process_mods mods =
      forM mods $ \modsum ->
        handleSourceError
          (\err -> do
             printExceptionAndWarnings err
             throwE ("Failed to check module: " ++ moduleString (ms_mod modsum))) $
        do
          liftIO $ putStrLn $ "Processing " ++ moduleString (ms_mod modsum)
          let filename = msHsFilePath modsum
          let flags = ms_hspp_opts modsum
          tc_mod <- loadModule =<< typecheckModule =<< parseModule modsum
          let Just renamed_src = renamedSource tc_mod
          return $ mkGhcModule (ms_mod modsum,
                                filename,
                                (parsedSource tc_mod,
                                 renamed_src,
                                 typecheckedSource tc_mod,
                                 moduleInfo tc_mod))
                                flags

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
