module GHC.Tc.TyCl.PatSyn where

import GHC.Hs    ( PatSynBind, LHsBinds )
import GHC.Tc.Types ( TcM, TcSigInfo )
import GHC.Tc.Utils.Monad ( TcGblEnv)
import GHC.Utils.Outputable ( Outputable )
import GHC.Hs.Extension ( GhcRn, GhcTc )
import Data.Maybe  ( Maybe )

tcPatSynDecl :: PatSynBind GhcRn GhcRn
             -> Maybe TcSigInfo
             -> TcM (LHsBinds GhcTc, TcGblEnv)

tcPatSynBuilderBind :: PatSynBind GhcRn GhcRn -> TcM (LHsBinds GhcTc)

nonBidirectionalErr :: Outputable name => name -> TcM a
