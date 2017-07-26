{-# LANGUAGE GADTs #-}
module OutputableAnnotation (PExpr(..), BindType, varBinder, varReference) where

import CoreSyn
import Outputable ( OutputableBndr(..))

data PExpr where
  PCoreExpr :: OutputableBndr a => Expr a -> PExpr
  PBind :: OutputableBndr a => Bind a -> PExpr
  PVar :: OutputableBndr a => BindType -> a -> PExpr

data BindType = Binder | Reference

varBinder :: OutputableBndr a => a -> PExpr
varBinder a = PVar Binder a

varReference :: OutputableBndr a => a -> PExpr
varReference a = PVar Reference a



