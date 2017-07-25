{-# LANGUAGE GADTs #-}
module OutputableAnnotation (PExpr(..)) where

import CoreSyn

data PExpr where
  PCoreExpr :: CoreExpr -> PExpr


