{-# LANGUAGE GADTs #-}
module OutputableAnnotation (PExpr(..), BindType(..), varBinder, varReference) where

import CoreSyn
import Outputable ( OutputableBndr(..))
import Name (NamedThing)

data PExpr where
  PCoreExpr :: NamedThing a => Expr a -> PExpr
  PBind :: NamedThing a => Bind a -> PExpr
  PVar :: NamedThing a => BindType -> a -> PExpr

data BindType = Binder | Reference

varBinder :: NamedThing a => a -> PExpr
varBinder a = PVar Binder a

varReference :: NamedThing a => a -> PExpr
varReference a = PVar Reference a



