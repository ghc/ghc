module GHC.Tc.Module where

import GHC.Prelude
import GHC.Core.Type(TyThing)
import GHC.Tc.Types (TcM)
import GHC.Utils.Outputable (SDoc)
import GHC.Types.Name (Name)

checkBootDeclM :: Bool  -- ^ True <=> an hs-boot file (could also be a sig)
               -> TyThing -> TyThing -> TcM ()
missingBootThing :: Bool -> Name -> String -> SDoc
badReexportedBootThing :: Bool -> Name -> Name -> SDoc
