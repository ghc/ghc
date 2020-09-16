module GHC.Core.DataCon where

import GHC.Prelude
import {-# SOURCE #-} GHC.Types.Var( Id, TyVar, TyCoVar, InvisTVBinder )
import {-# SOURCE #-} GHC.Types.Name( Name, NamedThing )
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
dataConWorkId    :: DataCon -> Id
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
isUnboxedSumDataCon :: DataCon -> Bool

instance Eq DataCon
instance Uniquable DataCon
instance NamedThing DataCon
instance Outputable DataCon
instance OutputableBndr DataCon

dataConWrapId :: DataCon -> Id
promoteDataCon :: DataCon -> TyCon
