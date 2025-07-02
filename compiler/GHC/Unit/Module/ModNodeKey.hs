module GHC.Unit.Module.ModNodeKey
  ( ModNodeKeyWithUid(..)
  , mnkToModule
  , moduleToMnk
  , mnkIsBoot
  , mnkToInstalledModule
  , installedModuleToMnk
  ) where

import GHC.Prelude
import GHC.Utils.Outputable
import GHC.Unit.Types

data ModNodeKeyWithUid = ModNodeKeyWithUid { mnkModuleName :: !ModuleNameWithIsBoot
                                           , mnkUnitId     :: !UnitId } deriving (Eq, Ord)

instance Show ModNodeKeyWithUid where
  show = showSDocUnsafe . ppr

mnkToModule :: ModNodeKeyWithUid -> Module
mnkToModule (ModNodeKeyWithUid mnwib uid) = Module (RealUnit (Definite uid)) (gwib_mod mnwib)

mnkToInstalledModule :: ModNodeKeyWithUid -> InstalledModule
mnkToInstalledModule (ModNodeKeyWithUid mnwib uid) = Module uid (gwib_mod mnwib)

-- | Already InstalledModules are always NotBoot
installedModuleToMnk :: InstalledModule -> ModNodeKeyWithUid
installedModuleToMnk mod = ModNodeKeyWithUid (GWIB (moduleName mod) NotBoot) (moduleUnit mod)

moduleToMnk :: Module -> IsBootInterface -> ModNodeKeyWithUid
moduleToMnk mod is_boot = ModNodeKeyWithUid (GWIB (moduleName mod) is_boot) (moduleUnitId mod)

mnkIsBoot :: ModNodeKeyWithUid -> IsBootInterface
mnkIsBoot (ModNodeKeyWithUid mnwib _) = gwib_isBoot mnwib

instance Outputable ModNodeKeyWithUid where
  ppr (ModNodeKeyWithUid mnwib uid) = ppr uid <> colon <> ppr mnwib

