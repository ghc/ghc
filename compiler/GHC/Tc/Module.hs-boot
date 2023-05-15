module GHC.Tc.Module where

import GHC.Types.SourceFile(HsBootOrSig)
import GHC.Types.TyThing(TyThing)
import GHC.Tc.Types (TcM)

checkBootDeclM :: HsBootOrSig -> TyThing -> TyThing -> TcM ()
