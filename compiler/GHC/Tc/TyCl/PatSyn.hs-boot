module GHC.Tc.TyCl.PatSyn where

import GHC.Hs    ( PatSynBind, LHsBinds )
import GHC.Tc.Types ( TcM, TcSigInfo )
import GHC.Tc.Utils.Monad ( TcGblEnv)
import GHC.Hs.Extension.GhcPass ( GhcRn, GhcTc )
import Data.Maybe  ( Maybe )
import GHC.Tc.Gen.Sig ( TcPragEnv )

tcPatSynDecl :: PatSynBind GhcRn GhcRn
             -> Maybe TcSigInfo
             -> TcPragEnv
             -> TcM (LHsBinds GhcTc, TcGblEnv)

tcPatSynBuilderBind :: PatSynBind GhcRn GhcRn -> TcM (LHsBinds GhcTc)

