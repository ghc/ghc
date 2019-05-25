module TcType where
import Outputable( SDoc' )

data MetaDetails

data TcTyVarDetails
pprTcTyVarDetails :: TcTyVarDetails -> SDoc' r
vanillaSkolemTv :: TcTyVarDetails
