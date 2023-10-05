module GHC.Tc.Utils.TcType where
import GHC.Utils.Outputable( SDoc )
import GHC.Prelude ( Bool )
import {-# SOURCE #-} GHC.Types.Var ( TcTyVar )
import {-# SOURCE #-} GHC.Tc.Types.Origin ( FixedRuntimeRepOrigin )
import GHC.Types.Name.Env ( NameEnv )
import GHC.Stack

data MetaDetails

data TcTyVarDetails
pprTcTyVarDetails :: TcTyVarDetails -> SDoc
vanillaSkolemTvUnk :: HasCallStack => TcTyVarDetails
isMetaTyVar :: TcTyVar -> Bool
isTyConableTyVar :: TcTyVar -> Bool

type ConcreteTyVars = NameEnv ConcreteTvOrigin
data ConcreteTvOrigin
  = ConcreteFRR FixedRuntimeRepOrigin

isConcreteTyVar :: TcTyVar -> Bool
noConcreteTyVars :: ConcreteTyVars
