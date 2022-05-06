module GHC.Tc.TyCl.PatSyn where

import GHC.Hs    ( PatSynBind, LHsBinds )
import GHC.Tc.Types ( TcM )
import GHC.Tc.Utils.Monad ( TcGblEnv)
import GHC.Hs.Extension ( GhcRn, GhcTc )
import GHC.Tc.Gen.Sig ( TcPragEnv, TcSigFun )
import GHC.Parser.Annotation( LocatedA )

tcPatSynDecl :: LocatedA (PatSynBind GhcRn GhcRn)
             -> TcSigFun
             -> TcPragEnv
             -> TcM (LHsBinds GhcTc, TcGblEnv)

tcPatSynBuilderBind :: TcPragEnv -> PatSynBind GhcRn GhcRn
                    -> TcM (LHsBinds GhcTc)

