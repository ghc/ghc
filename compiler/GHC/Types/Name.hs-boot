module GHC.Types.Name (
    module GHC.Types.Name,
    module GHC.Types.Name.Occurrence
) where

import GHC.Prelude (Eq, Bool)
import {-# SOURCE #-} GHC.Types.Name.Occurrence
import GHC.Types.Unique
import GHC.Utils.Outputable
import Data.Data (Data)
import Control.DeepSeq (NFData)

data Name

instance Eq Name
instance Data Name
instance Uniquable Name
instance Outputable Name
instance NFData Name

class NamedThing a where
    getOccName :: a -> OccName
    getName    :: a -> Name

    getOccName n = nameOccName (getName n)

nameUnique :: Name -> Unique
setNameUnique :: Name -> Unique -> Name
nameOccName :: Name -> OccName
tidyNameOcc :: Name -> OccName -> Name
isFieldName :: Name -> Bool
