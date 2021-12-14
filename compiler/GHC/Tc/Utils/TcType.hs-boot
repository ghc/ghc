module GHC.Tc.Utils.TcType where
import GHC.Utils.Outputable( SDoc )
import GHC.Prelude ( Bool )
import {-# SOURCE #-} GHC.Types.Var ( TcTyVar )
import {-# SOURCE #-} GHC.Core.TyCo.Rep
import {-# SOURCE #-} GHC.Tc.Types.Origin
import GHC.Utils.Misc ( HasDebugCallStack )
import GHC.Stack

data MetaDetails

data TcTyVarDetails
pprTcTyVarDetails :: TcTyVarDetails -> SDoc
vanillaSkolemTv :: SkolemInfo -> TcTyVarDetails
vanillaSkolemTvUnk :: HasCallStack => TcTyVarDetails
isMetaTyVar :: TcTyVar -> Bool
isTyConableTyVar :: TcTyVar -> Bool

tcEqType :: HasDebugCallStack => Type -> Type -> Bool

