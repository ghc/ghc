module GHC.Core.TyCoPpr where

import {-# SOURCE #-} GHC.Core.TyCoRep (Type, Kind, Coercion, TyLit)
import Outputable

pprType :: Type -> SDoc
pprKind :: Kind -> SDoc
pprCo :: Coercion -> SDoc
pprTyLit :: TyLit -> SDoc

