module DataCon where
import Var( TyVar, TyVarBinder )
import Name( Name, NamedThing )
import {-# SOURCE #-} TyCon( TyCon )
import FieldLabel ( FieldLabel )
import Unique ( Uniquable )
import Outputable ( Outputable, OutputableBndr )
import BasicTypes (Arity)
import {-# SOURCE #-} TyCoRep ( Type, ThetaType )
import Weight

data DataCon
data DataConRep
data EqSpec
filterEqSpec :: [EqSpec] -> [TyVarBinder] -> [TyVarBinder]

dataConName      :: DataCon -> Name
dataConTyCon     :: DataCon -> TyCon
dataConUnivTyVarBinders :: DataCon -> [TyVarBinder]
dataConExTyVars  :: DataCon -> [TyVar]
dataConExTyVarBinders :: DataCon -> [TyVarBinder]
dataConSourceArity  :: DataCon -> Arity
dataConFieldLabels :: DataCon -> [FieldLabel]
dataConInstOrigArgTys  :: DataCon -> [Type] -> [Type]
dataConStupidTheta :: DataCon -> ThetaType
dataConFullSig :: DataCon
               -> ([TyVar], [TyVar], [EqSpec], ThetaType, [Weighted Type], Type)

instance Eq DataCon
instance Uniquable DataCon
instance NamedThing DataCon
instance Outputable DataCon
instance OutputableBndr DataCon
