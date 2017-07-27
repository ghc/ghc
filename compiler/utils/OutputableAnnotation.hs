{-# LANGUAGE GADTs #-}
module OutputableAnnotation (PExpr(..), BindType(..), varBinder, varReference) where

import CoreSyn
import Outputable ( OutputableBndr(..))
import Name (NamedThing)

data PExpr where
  PCoreExpr :: (OutputableBndr a, NamedThing a) => Expr a -> PExpr
  PBind :: (OutputableBndr a, NamedThing a) => Bind a -> PExpr
  PVar :: (OutputableBndr a, NamedThing a) => BindType -> a -> PExpr

data BindType = Binder | Reference

varBinder :: (OutputableBndr a, NamedThing a) => a -> PExpr
varBinder a = PVar Binder a

varReference :: (OutputableBndr a, NamedThing a) => a -> PExpr
varReference a = PVar Reference a



