module GHC.Core.TyCo.Ppr where

import {-# SOURCE #-} GHC.Core.TyCo.Rep (Type, Kind, Coercion, TyLit)
import GHC.Utils.Outputable

pprType :: Type -> SDoc
pprKind :: Kind -> SDoc
pprCo :: Coercion -> SDoc
pprTyLit :: TyLit -> SDoc

