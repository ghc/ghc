{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}
module Bar (data PointerExpr) where

import Data.Kind (Type)
import T15186A

-------------------------------------------------------------------------------

pattern PointerExpr :: Expr tp
pattern PointerExpr <-
   App (RollRecursive (EmptyAssn :> BVRepr) (App _))

-------------------------------------------------------------------------------

data CrucibleType where
  RecursiveType :: Ctx CrucibleType -> CrucibleType

data TypeRepr (tp :: CrucibleType) where
  BVRepr :: TypeRepr tp
  TypeReprDummy :: TypeRepr tp

data App (f :: CrucibleType -> Type) (tp :: CrucibleType) where
  RollRecursive :: !(Assignment TypeRepr ctx)
                -> !(Expr tp)
                -> App f ('RecursiveType ctx)

data Expr (tp :: CrucibleType)
  = App !(App Expr tp)
  | ExprDummy
