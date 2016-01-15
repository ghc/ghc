module TcPatSyn where

import Name      ( Name )
import Id        ( Id )
import HsSyn     ( PatSynBind, LHsBinds, LHsSigType )
import TcRnTypes ( TcM, TcSigFun, TcPatSynInfo )
import TcRnMonad ( TcGblEnv)
import Outputable ( Outputable )

tcPatSynSig :: Name -> LHsSigType Name
            -> TcM TcPatSynInfo

tcInferPatSynDecl :: PatSynBind Name Name
                  -> TcM (LHsBinds Id, TcGblEnv)

tcCheckPatSynDecl :: PatSynBind Name Name
                  -> TcPatSynInfo
                  -> TcM (LHsBinds Id, TcGblEnv)

tcPatSynBuilderBind :: TcSigFun -> PatSynBind Name Name
                    -> TcM (LHsBinds Id)

nonBidirectionalErr :: Outputable name => name -> TcM a
