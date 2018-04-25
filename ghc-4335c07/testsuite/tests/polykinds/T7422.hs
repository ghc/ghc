{-# LANGUAGE StandaloneDeriving, DataKinds, KindSignatures, DeriveGeneric, GADTs#-}

module T7422 where

import GHC.Generics

type CellId = Int
type Ident = String
type LabelName = String

data ExprTag = TagAst | TagEval

data Expr (tag :: ExprTag) where
  Var :: Ident -> Expr tag
  Label :: LabelName -> Expr tag -> Expr tag

deriving instance Generic (Expr a)
