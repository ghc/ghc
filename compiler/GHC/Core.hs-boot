{-# LANGUAGE NoPolyKinds #-}
module GHC.Core where
import GHC.Prelude ( Maybe )
import {-# SOURCE #-} GHC.Types.Var
import {-# SOURCE #-} GHC.Core.TyCo.Rep ( Type )

data Expr a

type CoreBndr = Var

type CoreExpr = Expr CoreBndr

varToCoreExpr :: CoreBndr -> Expr b
exprToBoolIndex :: CoreExpr -> Maybe Type
mapBoolIndex :: (Type -> Type) -> CoreExpr -> CoreExpr
