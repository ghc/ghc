--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.GHC.Typecheck (
  mkGhcModule
) where


import Haddock.Types

import Data.Maybe
import GHC
import SrcLoc

import Data.List


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
    (parsed, renamed, _, modInfo) = checkedMod
