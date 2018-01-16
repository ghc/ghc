{-# LANGUAGE GADTs, ExistentialQuantification, KindSignatures, RankNTypes #-}

-- Succeeds (needs the (Ord a) in TypeSet 
-- c.f. gadt21.hs

-- However, it's a useful test because it unearthed a bug
-- in free-variable-finding

module Expr where

import Data.Set (Set)

data Type a where
    TypeInt     :: Type Int
    TypeSet     :: Ord a => Type a -> Type (Set a)
    TypeFun     :: Type a -> Type b -> Type (a -> b)

data Expr :: * -> * where
    Const :: Type a -> a -> Expr a

data DynExpr = forall a. DynExpr (Expr a)

withOrdDynExpr :: DynExpr -> (forall a. Ord a => Expr a -> b) -> Maybe b
withOrdDynExpr (DynExpr e@(Const (TypeSet _) _)) f = Just (f e)
withOrdDynExpr (DynExpr e@(Const TypeInt _)) f = Just (f e)
withOrdDynExpr _ _ = Nothing
