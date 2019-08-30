module TyCoPpr where

import {-# SOURCE #-} TyCoRep (Type, Kind, Coercion, TyLit)
import Outputable ( SDoc', HasPprConfig )
import {-# SOURCE #-} Packages (HasPackageState)
import NameSuppress ( HasNameSuppress )
import TypeSuppress

pprKind
  :: ( HasPprConfig r
     , HasNameSuppress r
     , HasTypeSuppress r
     , HasPackageState r
     )
  => Kind -> SDoc' r
pprType
  :: ( HasPprConfig r
     , HasNameSuppress r
     , HasTypeSuppress r
     , HasPackageState r
     )
  => Type -> SDoc' r
pprCo
  :: ( HasPprConfig r
     , HasNameSuppress r
     , HasTypeSuppress r
     , HasPackageState r
     )
  => Coercion -> SDoc' r
pprTyLit
  :: TyLit -> SDoc' r
