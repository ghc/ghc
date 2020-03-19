module GHC.Tc.PatSyn where

import GHC.Hs    ( PatSynBind, LHsBinds )
import GHC.Tc.Utils ( TcM, TcSigInfo )
import GHC.Tc.Monad ( TcGblEnv)
import Outputable ( Outputable )
import GHC.Hs.Extension ( GhcRn, GhcTc )
import Data.Maybe  ( Maybe )

tcPatSynDecl :: PatSynBind GhcRn GhcRn
             -> Maybe TcSigInfo
             -> TcM (LHsBinds GhcTc, TcGblEnv)

tcPatSynBuilderBind :: PatSynBind GhcRn GhcRn -> TcM (LHsBinds GhcTc)

nonBidirectionalErr :: Outputable name => name -> TcM a
