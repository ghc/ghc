-- Breaks the recursive loop between
--   GHC.Core.TyCo.Make and
--   GHC.Builtin.Types.Prim

module GHC.Core.TyCo.Make where

import GHC.Core.TyCo.Rep(Type)
import GHC.Core.TyCon(TyCon)

mkTyConApp :: TyCon -> [Type] -> Type
