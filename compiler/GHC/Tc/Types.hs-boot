module GHC.Tc.Types where

import GHC.Prelude
import GHC.Tc.Utils.TcType
import GHC.Types.SrcLoc

data TcLclEnv

data SelfBootInfo

setLclEnvTcLevel :: TcLclEnv -> TcLevel -> TcLclEnv
getLclEnvTcLevel :: TcLclEnv -> TcLevel

setLclEnvLoc :: TcLclEnv -> RealSrcSpan -> TcLclEnv
getLclEnvLoc :: TcLclEnv -> RealSrcSpan

lclEnvInGeneratedCode :: TcLclEnv -> Bool
