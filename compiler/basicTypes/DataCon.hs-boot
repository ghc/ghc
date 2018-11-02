module DataCon where

import GhcPrelude
import Var( TyVar, TyVarBinder )
import Name( Name, NamedThing )
import {-# SOURCE #-} TyCon( TyCon )
import FieldLabel ( FieldLabel )
import Unique ( Uniquable )
import Outputable ( Outputable, OutputableBndr )
import BasicTypes (Arity)
import {-# SOURCE #-} TyCoRep ( Type, ThetaType, Weighted )
import Util (  HasCallStack )

data DataCon
data DataConRep
data EqSpec

dataConName      :: DataCon -> Name
dataConTyCon     :: DataCon -> TyCon
dataConExTyVars  :: DataCon -> [TyVar]
dataConUserTyVars :: DataCon -> [TyVar]
dataConUserTyVarBinders :: DataCon -> [TyVarBinder]
dataConSourceArity  :: DataCon -> Arity
dataConFieldLabels :: DataCon -> [FieldLabel]
dataConInstOrigArgTys  :: HasCallStack => DataCon -> [Type] -> [Weighted Type]
dataConStupidTheta :: DataCon -> ThetaType
dataConFullSig :: DataCon
               -> ([TyVar], [TyVar], [EqSpec], ThetaType, [Weighted Type], Type)
isUnboxedSumCon :: DataCon -> Bool

instance Eq DataCon
instance Uniquable DataCon
instance NamedThing DataCon
instance Outputable DataCon
instance OutputableBndr DataCon
