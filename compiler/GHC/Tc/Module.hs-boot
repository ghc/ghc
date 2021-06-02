module GHC.Tc.Module where

import GHC.Prelude
import GHC.Types.TyThing(TyThing)
import GHC.Tc.Errors.Types (TcRnMessage)
import GHC.Tc.Types (TcM)
import GHC.Types.Name (Name)

checkBootDeclM :: Bool  -- ^ True <=> an hs-boot file (could also be a sig)
               -> TyThing -> TyThing -> TcM ()
missingBootThing :: Bool -> Name -> String -> TcRnMessage
badReexportedBootThing :: Bool -> Name -> Name -> TcRnMessage
