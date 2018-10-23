module TcPatSyn where

import HsSyn     ( PatSynBind, LHsBinds )
import TcRnTypes ( TcM, TcSigInfo )
import TcRnMonad ( TcGblEnv)
import Outputable ( Outputable )
import HsExtension ( GhcRn, GhcTc )
import Data.Maybe  ( Maybe )

tcPatSynDecl :: PatSynBind GhcRn GhcRn
             -> Maybe TcSigInfo
             -> TcM (LHsBinds GhcTc, TcGblEnv)

tcPatSynBuilderBind :: PatSynBind GhcRn GhcRn -> TcM (LHsBinds GhcTc)

nonBidirectionalErr :: Outputable name => name -> TcM a
