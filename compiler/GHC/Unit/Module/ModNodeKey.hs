module GHC.Unit.Module.ModNodeKey ( ModNodeKeyWithUid(..), mnkToModule, mnkIsBoot ) where

import GHC.Prelude
import GHC.Utils.Outputable
import GHC.Unit.Types

data ModNodeKeyWithUid = ModNodeKeyWithUid { mnkModuleName :: !ModuleNameWithIsBoot
                                           , mnkUnitId     :: !UnitId } deriving (Eq, Ord)

mnkIsBoot :: ModNodeKeyWithUid -> IsBootInterface
mnkIsBoot (ModNodeKeyWithUid mnwib _) = gwib_isBoot mnwib

mnkToModule :: ModNodeKeyWithUid -> Module
mnkToModule (ModNodeKeyWithUid mnwib uid) = Module (RealUnit (Definite uid)) (gwib_mod mnwib)

instance Outputable ModNodeKeyWithUid where
  ppr (ModNodeKeyWithUid mnwib uid) = ppr uid <> colon <> ppr mnwib

