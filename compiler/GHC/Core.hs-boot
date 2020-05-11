module GHC.Core where

import GHC.Types.Var

data Expr a

type CoreBndr = Var
-- | Expressions where binders are 'CoreBndr's
type CoreExpr = Expr CoreBndr