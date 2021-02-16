module TcType where
import Outputable( SDoc )
import {-# SOURCE #-} TyCoRep( Type )
import {-# SOURCE #-} TyCon (TyCon)
import Data.Maybe (Maybe)

data MetaDetails

data TcTyVarDetails
pprTcTyVarDetails :: TcTyVarDetails -> SDoc
vanillaSkolemTv :: TcTyVarDetails
tcSplitIOType_maybe :: Type -> Maybe (TyCon, Type)
