module GHC.Tc.Types where

import GHC.Tc.Utils.Type
import GHC.Types.SrcLoc

data TcLclEnv

setLclEnvTcLevel :: TcLclEnv -> TcLevel -> TcLclEnv
getLclEnvTcLevel :: TcLclEnv -> TcLevel

setLclEnvLoc :: TcLclEnv -> RealSrcSpan -> TcLclEnv
getLclEnvLoc :: TcLclEnv -> RealSrcSpan
