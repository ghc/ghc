module TyCoPpr where

import {-# SOURCE #-} TyCoRep (Type, Kind, Coercion, TyLit)
import Outputable

pprType :: Type -> SDoc
pprKind :: Kind -> SDoc
pprCo :: Coercion -> SDoc
pprTyLit :: TyLit -> SDoc

