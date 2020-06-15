module GHC.Core.DataCon where

import GHC.Prelude
import GHC.Types.Var( TyVar, TyCoVar, InvisTVBinder )
import GHC.Types.Name( Name, NamedThing )
import {-# SOURCE #-} GHC.Core.TyCon( TyCon )
import GHC.Types.FieldLabel ( FieldLabel )
import GHC.Types.Unique ( Uniquable )
import GHC.Utils.Outputable ( Outputable, OutputableBndr )
import GHC.Types.Basic (Arity)
import {-# SOURCE #-} GHC.Core.TyCo.Rep ( Type, ThetaType, Scaled )

data DataCon
data DataConRep
data EqSpec

dataConName      :: DataCon -> Name
dataConTyCon     :: DataCon -> TyCon
dataConExTyCoVars :: DataCon -> [TyCoVar]
dataConUserTyVars :: DataCon -> [TyVar]
dataConUserTyVarBinders :: DataCon -> [InvisTVBinder]
dataConSourceArity  :: DataCon -> Arity
dataConFieldLabels :: DataCon -> [FieldLabel]
dataConInstOrigArgTys  :: DataCon -> [Type] -> [Scaled Type]
dataConStupidTheta :: DataCon -> ThetaType
dataConFullSig :: DataCon
               -> ([TyVar], [TyCoVar], [EqSpec], ThetaType, [Scaled Type], Type)
isUnboxedSumCon :: DataCon -> Bool

instance Eq DataCon
instance Uniquable DataCon
instance NamedThing DataCon
instance Outputable DataCon
instance OutputableBndr DataCon
