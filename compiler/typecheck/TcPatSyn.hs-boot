module TcPatSyn where

import Name      ( Name )
import Id        ( Id )
import HsSyn     ( PatSynBind, LHsBinds )
import TcRnTypes ( TcM, TcPatSynInfo )
import PatSyn    ( PatSyn )
import TcRnMonad ( TcGblEnv )
import Outputable ( Outputable )

tcInferPatSynDecl :: PatSynBind Name Name
                  -> TcM (PatSyn, LHsBinds Id, TcGblEnv)

tcCheckPatSynDecl :: PatSynBind Name Name
                  -> TcPatSynInfo
                  -> TcM (PatSyn, LHsBinds Id, TcGblEnv)

tcPatSynBuilderBind :: PatSynBind Name Name
                    -> TcM (LHsBinds Id)

nonBidirectionalErr :: Outputable name => name -> TcM a
