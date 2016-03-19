module DataCon where
import Var( TyVar )
import Name( Name, NamedThing )
import {-# SOURCE #-} TyCon( TyCon )
import FieldLabel ( FieldLabel )
import Unique ( Uniquable )
import Outputable ( Outputable, OutputableBndr )
import BasicTypes (Arity)
import {-# SOURCE #-} TyCoRep (Type, ThetaType, TyBinder)

data DataCon
data DataConRep
data EqSpec
filterEqSpec :: [EqSpec] -> [TyBinder] -> [TyBinder]

dataConName      :: DataCon -> Name
dataConTyCon     :: DataCon -> TyCon
dataConUnivTyBinders :: DataCon -> [TyBinder]
dataConExTyVars  :: DataCon -> [TyVar]
dataConExTyBinders :: DataCon -> [TyBinder]
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
