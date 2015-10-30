module ConLike where
import Data.Typeable
import Name (NamedThing)
import {-# SOURCE #-} DataCon (DataCon)
import {-# SOURCE #-} PatSyn (PatSyn)
import Outputable
import Data.Data (Data)

data ConLike = RealDataCon DataCon
             | PatSynCon PatSyn

instance Eq ConLike
instance Typeable ConLike
instance Ord ConLike
instance NamedThing ConLike
instance Data ConLike
instance Outputable ConLike
instance OutputableBndr ConLike
