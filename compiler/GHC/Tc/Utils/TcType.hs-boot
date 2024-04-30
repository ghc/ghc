module GHC.Tc.Utils.TcType where
import GHC.Utils.Outputable( SDoc )
import GHC.Utils.Misc( HasDebugCallStack )
import GHC.Prelude ( Bool )
import {-# SOURCE #-} GHC.Types.Var ( TcTyVar )
import {-# SOURCE #-} GHC.Tc.Types.Origin ( FixedRuntimeRepOrigin )
import GHC.Types.Name.Env ( NameEnv )

data MetaDetails

data TcTyVarDetails
pprTcTyVarDetails :: TcTyVarDetails -> SDoc
vanillaSkolemTvUnk :: HasDebugCallStack => TcTyVarDetails
isMetaTyVar :: TcTyVar -> Bool
isTyConableTyVar :: TcTyVar -> Bool

type ConcreteTyVars = NameEnv ConcreteTvOrigin
data ConcreteTvOrigin
  = ConcreteFRR FixedRuntimeRepOrigin

isConcreteTyVar :: TcTyVar -> Bool
noConcreteTyVars :: ConcreteTyVars
