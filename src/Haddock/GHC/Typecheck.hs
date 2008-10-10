--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.GHC.Typecheck (
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

import Data.List


type CheckedMod = (Module, FilePath, FullyCheckedMod)


type FullyCheckedMod = (ParsedSource, 
                        RenamedSource, 
                        TypecheckedSource, 
                        ModuleInfo)


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
