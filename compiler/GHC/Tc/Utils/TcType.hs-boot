module GHC.Tc.Utils.TcType where
import GHC.Utils.Outputable( SDoc )
import GHC.Prelude ( Bool )
import {-# SOURCE #-} GHC.Types.Var ( TcTyVar )
import GHC.Utils.Misc( HasDebugCallStack )

data MetaDetails

data TcTyVarDetails
pprTcTyVarDetails :: TcTyVarDetails -> SDoc
vanillaSkolemTvUnk :: HasDebugCallStack => TcTyVarDetails
isMetaTyVar :: TcTyVar -> Bool
isTyConableTyVar :: TcTyVar -> Bool
isConcreteTyVar :: TcTyVar -> Bool

