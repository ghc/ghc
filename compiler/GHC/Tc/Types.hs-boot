module GHC.Tc.Types where

import GHC.Prelude
import GHC.Tc.Utils.TcType
import GHC.Types.SrcLoc
import GHC.Utils.Outputable

data TcLclEnv

data SelfBootInfo

data TcIdSigInfo
instance Outputable TcIdSigInfo

data TcTyThing
instance Outputable TcTyThing

data SpliceType = Typed | Untyped
data SpliceOrBracket = IsSplice | IsBracket

setLclEnvTcLevel :: TcLclEnv -> TcLevel -> TcLclEnv
getLclEnvTcLevel :: TcLclEnv -> TcLevel

setLclEnvLoc :: TcLclEnv -> RealSrcSpan -> TcLclEnv
getLclEnvLoc :: TcLclEnv -> RealSrcSpan

lclEnvInGeneratedCode :: TcLclEnv -> Bool
pprTcTyThingCategory :: TcTyThing -> SDoc
