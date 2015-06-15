module DataCon where
import Var( TyVar )
import Name( Name, NamedThing )
import {-# SOURCE #-} TyCon( TyCon )
import FieldLabel ( FieldLabel )
import Unique ( Uniquable )
import Outputable ( Outputable, OutputableBndr )

data DataCon
data DataConRep
dataConName      :: DataCon -> Name
dataConTyCon     :: DataCon -> TyCon
dataConExTyVars  :: DataCon -> [TyVar]
dataConFieldLabels :: DataCon -> [FieldLabel]

instance Eq DataCon
instance Ord DataCon
instance Uniquable DataCon
instance NamedThing DataCon
instance Outputable DataCon
instance OutputableBndr DataCon
