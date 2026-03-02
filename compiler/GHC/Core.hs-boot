{-# LANGUAGE NoPolyKinds #-}
module GHC.Core where
import {-# SOURCE #-} GHC.Types.Var

data Expr a

type CoreBndr = Var

type CoreExpr = Expr CoreBndr

data CoreCompUnit
type CoreProgram = [CoreCompUnit]
