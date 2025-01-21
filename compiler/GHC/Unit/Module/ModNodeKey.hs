module GHC.Unit.Module.ModNodeKey ( ModNodeKeyWithUid(..) ) where

import GHC.Prelude
import GHC.Utils.Outputable
import GHC.Unit.Types

data ModNodeKeyWithUid = ModNodeKeyWithUid { mnkModuleName :: !ModuleNameWithIsBoot
                                           , mnkUnitId     :: !UnitId } deriving (Eq, Ord)

instance Outputable ModNodeKeyWithUid where
  ppr (ModNodeKeyWithUid mnwib uid)
    = ppr uid <> colon <> ppr mnwib

