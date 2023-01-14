{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE FlexibleContexts #-}
module Telomare.Possible where

import Data.Kind (Type)

data PartExprF f
  = ZeroSF
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype EnhancedExpr f = EnhancedExpr {unEnhanceExpr :: SplitFunctor f PartExprF (EnhancedExpr f)} -- deriving (Eq, Show)

type family Base t :: Type -> Type

type instance Base (EnhancedExpr f) = SplitFunctor f PartExprF

class Functor (Base t) => Recursive t where
  project :: t -> Base t t

instance Functor f => Recursive (EnhancedExpr f) where
  project = unEnhanceExpr

class Functor (Base t) => Corecursive t where
  embed :: Base t t -> t

instance Functor f => Corecursive (EnhancedExpr f) where
  embed = EnhancedExpr

type SimpleExpr = EnhancedExpr VoidF
type BasicBase f = SplitFunctor f PartExprF
type SuperBase f = BasicBase (SplitFunctor f SuperPositionF)
type AbortBase f = SuperBase (SplitFunctor f AbortableF)
type UnsizedBase = AbortBase UnsizedRecursionF

pattern UnsizedFW :: UnsizedRecursionF a -> UnsizedBase a
pattern UnsizedFW x = SplitFunctor (Left (SplitFunctor (Left (SplitFunctor (Left x)))))
pattern BasicExpr :: PartExprF (EnhancedExpr f) -> EnhancedExpr f
pattern BasicExpr x = EnhancedExpr (SplitFunctor (Right x))
pattern UnsizedWrap :: UnsizedRecursionF UnsizedExpr -> UnsizedExpr
pattern UnsizedWrap x = EnhancedExpr (UnsizedFW x)

data VoidF f
  deriving (Functor, Foldable, Traversable)

data SuperPositionF f
  = AnyPF
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data AbortableF f
  = AbortF
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype SplitFunctor g f x = SplitFunctor { unSplitF :: Either (g x) (f x) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (SplitFunctor g f) where

instance (Foldable f, Foldable g) => Foldable (SplitFunctor g f) where

instance (Traversable f, Traversable g) => Traversable (SplitFunctor g f) where

type SuperExpr f = EnhancedExpr (SplitFunctor f SuperPositionF)

type AbortExpr f = SuperExpr (SplitFunctor f AbortableF)

type BreakExtras = ()

data UnsizedRecursionF f
  = UnsizedRecursionF BreakExtras f
  | UnsizedBarrierF f
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type UnsizedExpr = AbortExpr UnsizedRecursionF

cata :: Recursive t => (Base t a -> a) -> t -> a
cata = undefined

sizeTerm :: UnsizedExpr -> Maybe (AbortExpr VoidF)
sizeTerm term =
  let sizingTerm = eval term
      eval :: UnsizedExpr -> UnsizedExpr
      eval = undefined
      setSizes sizes = cata $ \case
        UnsizedFW (UnsizedRecursionF be env) -> BasicExpr ZeroSF
      clean = undefined
      hoist = undefined
      maybeSized = pure sizingTerm
  in hoist clean <$> maybeSized


