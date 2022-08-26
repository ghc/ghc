module GHC.Tc.Utils.TcType where
import GHC.Utils.Outputable( SDoc )
import GHC.Prelude ( Bool )
import {-# SOURCE #-} GHC.Types.Var ( TcTyVar )
import GHC.Stack

data MetaDetails

data TcTyVarDetails
pprTcTyVarDetails :: TcTyVarDetails -> SDoc
vanillaSkolemTvUnk :: HasCallStack => TcTyVarDetails
isMetaTyVar :: TcTyVar -> Bool
isTyConableTyVar :: TcTyVar -> Bool
isConcreteTyVar :: TcTyVar -> Bool

