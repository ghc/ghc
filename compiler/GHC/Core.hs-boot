{-# LANGUAGE NoPolyKinds #-}
module GHC.Core where
import {-# SOURCE #-} GHC.Types.Var

data Expr a
data Bind b

type CoreBndr = Var

type CoreExpr = Expr CoreBndr
type CoreBind = Bind CoreBndr

data CoreCompUnit
type CoreProgram = [CoreCompUnit]
