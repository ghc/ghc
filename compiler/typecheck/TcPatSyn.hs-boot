module TcPatSyn where

import Name      ( Name )
import Id        ( Id )
import HsSyn     ( PatSynBind, LHsBinds )
import TcRnTypes ( TcM )
import PatSyn    ( PatSyn )
import TcPat     ( TcPatSynInfo )

tcInferPatSynDecl :: PatSynBind Name Name
                  -> TcM (PatSyn, LHsBinds Id)

tcCheckPatSynDecl :: PatSynBind Name Name
                  -> TcPatSynInfo
                  -> TcM (PatSyn, LHsBinds Id)

tcPatSynBuilderBind :: PatSynBind Name Name
                    -> TcM (LHsBinds Id)
