module DataCon where
import Var( TyVar )
import Name( Name, NamedThing )
import {-# SOURCE #-} TyCon( TyCon )
import FieldLabel ( FieldLabel )
import Unique ( Uniquable )
import Outputable ( Outputable, OutputableBndr )
import BasicTypes (Arity)
import {-# SOURCE #-} TyCoRep (Type, ThetaType)

data DataCon
data DataConRep
data EqSpec
eqSpecTyVar :: EqSpec -> TyVar

dataConName      :: DataCon -> Name
dataConTyCon     :: DataCon -> TyCon
dataConExTyVars  :: DataCon -> [TyVar]
dataConSourceArity  :: DataCon -> Arity
dataConFieldLabels :: DataCon -> [FieldLabel]
dataConInstOrigArgTys  :: DataCon -> [Type] -> [Type]
dataConStupidTheta :: DataCon -> ThetaType
dataConFullSig :: DataCon
               -> ([TyVar], [TyVar], [EqSpec], ThetaType, [Type], Type)

instance Eq DataCon
instance Ord DataCon
instance Uniquable DataCon
instance NamedThing DataCon
instance Outputable DataCon
instance OutputableBndr DataCon
