{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
module T16326_Compile1 where

import Data.Kind

type DApply a (b :: a -> Type) (f :: forall (x :: a) -> b x) (x :: a) =
  f x

type DComp a
           (b :: a -> Type)
           (c :: forall (x :: a). b x -> Type)
           (f :: forall (x :: a). forall (y :: b x) -> c y)
           (g :: forall (x :: a) -> b x)
           (x :: a) =
  f (g x)

type ElimList :: forall (a :: Type) (p :: [a] -> Type) (s :: [a]) ->
  p '[] -> (forall (x :: a) (xs :: [a]) -> p xs -> p (x:xs)) -> p s
type family ElimList a p s pNil pCons where
  forall a p pNil (pCons :: forall (x :: a) (xs :: [a]) -> p xs -> p (x:xs)).
    ElimList a p '[] pNil pCons =
      pNil
  forall a p x xs pNil (pCons :: forall (x :: a) (xs :: [a]) -> p xs -> p (x:xs)).
    ElimList a p (x:xs) pNil pCons =
      pCons x xs (ElimList a p xs pNil pCons)

data Proxy' :: forall k -> k -> Type where
  MkProxy' :: forall k (a :: k). Proxy' k a

type family Proxy2' ∷ ∀ k → k → Type where
  Proxy2' = Proxy'
