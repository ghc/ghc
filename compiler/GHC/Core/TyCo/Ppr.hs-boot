module GHC.Core.TyCo.Ppr where

import {-# SOURCE #-} GHC.Types.Var ( TyVar )
import {-# SOURCE #-} GHC.Core.TyCo.Rep (Type, Kind, Coercion, TyLit)
import GHC.Utils.Outputable ( SDoc )

pprType :: Type -> SDoc
pprKind :: Kind -> SDoc
pprCo :: Coercion -> SDoc
pprTyLit :: TyLit -> SDoc
pprTyVar :: TyVar -> SDoc
