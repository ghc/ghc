module GHC.Tc.Types where

import GHC.Tc.Utils.TcType
import GHC.Types.SrcLoc
import GHC.Utils.Outputable

data TcLclEnv

data SelfBootInfo

data TcIdSigInfo
instance Outputable TcIdSigInfo

setLclEnvTcLevel :: TcLclEnv -> TcLevel -> TcLclEnv
getLclEnvTcLevel :: TcLclEnv -> TcLevel

setLclEnvLoc :: TcLclEnv -> RealSrcSpan -> TcLclEnv
getLclEnvLoc :: TcLclEnv -> RealSrcSpan
