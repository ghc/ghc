module TcType where
import Outputable( SDoc )
import Prelude( Bool )

data MetaDetails

data TcTyVarDetails
pprTcTyVarDetails :: TcTyVarDetails -> SDoc
vanillaSkolemTv :: TcTyVarDetails
isMetaTyVarDetails :: TcTyVarDetails -> Bool
