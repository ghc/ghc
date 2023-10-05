module GHC.Types.TyThing.Ppr (
        pprTyThing,
        pprTyThingInContext
  ) where

import GHC.Iface.Type       ( ShowSub )
import GHC.Types.TyThing    ( TyThing )
import GHC.Utils.Outputable ( SDoc )

pprTyThing :: ShowSub -> TyThing -> SDoc
pprTyThingInContext :: ShowSub -> TyThing -> SDoc
