module GHC.Tc.Utils.TcType where
import GHC.Utils.Outputable( SDoc )

data MetaDetails

data TcTyVarDetails
pprTcTyVarDetails :: TcTyVarDetails -> SDoc
vanillaSkolemTv :: TcTyVarDetails
