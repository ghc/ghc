module GHC.Tc.Types.CtLocEnv where

import GHC.Prelude
import GHC.Types.SrcLoc
import GHC.Tc.Types.TcBinder
import GHC.Tc.Utils.TcType
import GHC.Types.Name.Reader
import GHC.Tc.Types.ErrCtxt


data CtLocEnv = CtLocEnv { ctl_ctxt :: ![ErrCtxt]
                         , ctl_loc :: !RealSrcSpan
                         , ctl_bndrs :: !TcBinderStack
                         , ctl_tclvl :: !TcLevel
                         , ctl_in_gen_code :: !Bool
                         , ctl_rdr :: !LocalRdrEnv }


getCtLocEnvLoc :: CtLocEnv -> RealSrcSpan
getCtLocEnvLoc = ctl_loc

getCtLocEnvLvl :: CtLocEnv -> TcLevel
getCtLocEnvLvl = ctl_tclvl

setCtLocEnvLvl :: CtLocEnv -> TcLevel -> CtLocEnv
setCtLocEnvLvl env lvl = env { ctl_tclvl = lvl }

setCtLocRealLoc :: CtLocEnv -> RealSrcSpan -> CtLocEnv
setCtLocRealLoc env ss = env { ctl_loc = ss }

setCtLocEnvLoc :: CtLocEnv -> SrcSpan -> CtLocEnv
-- See Note [Error contexts in generated code]
-- for the ctl_in_gen_code manipulation
setCtLocEnvLoc env (RealSrcSpan loc _)
  = env { ctl_loc = loc, ctl_in_gen_code = False }

setCtLocEnvLoc env loc@(UnhelpfulSpan _)
  | isGeneratedSrcSpan loc
  = env { ctl_in_gen_code = True }
  | otherwise
  = env

ctLocEnvInGeneratedCode :: CtLocEnv -> Bool
ctLocEnvInGeneratedCode = ctl_in_gen_code