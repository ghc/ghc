module GHC.Tc.TyCl.Utils where

import GHC.Core.TyCon (TyCon)
import GHC.Types.FieldLabel (FieldLabel, FieldBinds)
import GHC.Tc.Utils.Monad (TcM)
import GHC.Types.Var (Id)
import Language.Haskell.Syntax.Binds (LHsBind)
import GHC.Hs.Extension (GhcRn)
import GHC.Core.Type (Type)

mkSetFieldBinds :: TyCon -> FieldLabel -> TcM ( FieldBinds (Id, LHsBind GhcRn) )

mkRecordSetterType :: Type -> Type -> Type

mkRecordModifierType :: Type -> Type -> Type
