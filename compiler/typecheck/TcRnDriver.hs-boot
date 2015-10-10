module TcRnDriver where

import Type (TyThing)
import TcRnTypes (TcM)
import Outputable (SDoc)
import Name (Name)

checkBootDeclM :: Bool  -- ^ True <=> an hs-boot file (could also be a sig)
               -> TyThing -> TyThing -> TcM ()
missingBootThing :: Bool -> Name -> String -> SDoc
badReexportedBootThing :: Bool -> Name -> Name -> SDoc
