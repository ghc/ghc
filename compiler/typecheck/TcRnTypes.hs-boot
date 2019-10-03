module TcRnTypes where

import TcType
import SrcLoc

data TcLclEnv

setLclEnvTcLevel :: TcLclEnv -> TcLevel -> TcLclEnv
setLclEnvLoc :: TcLclEnv -> RealSrcSpan -> TcLclEnv

tcl_tclvl :: TcLclEnv -> TcLevel
tcl_loc :: TcLclEnv -> RealSrcSpan
