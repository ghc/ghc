{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             FlexibleInstances, GADTs, MultiParamTypeClasses,
             PolyKinds, ScopedTypeVariables, TypeFamilies,
             TypeOperators #-}
module T13025a where

data Nat = Z | S Nat
data Proxy a = Proxy

data Field :: (k,*) -> * where
  Field :: a -> Field '(s,a)

type family Index r rs :: Nat where
  Index r (r ': rs) = 'Z
  Index r (s ': rs) = 'S (Index r rs)

data Rec (rs :: [ (k,*) ]) where
  Nil :: Rec '[]
  (:&) :: Field r -> Rec rs -> Rec (r ': rs)
infixr 5 :&

class Index r rs ~ i => HasField r rs i where
  get :: proxy r -> Rec rs -> Field r
  set :: Field r -> Rec rs -> Rec rs

instance HasField r (r ': rs) 'Z where
  get _ (x :& _) = x
  set x (_ :& xs) = x :& xs

instance (HasField r rs i, Index r (s ': rs) ~ 'S i)
         => HasField r (s ': rs) ('S i) where
  get p (_ :& xs) = get p xs
  set x' (x :& xs) = x :& set x' xs

type Has r rs = HasField r rs (Index r rs)

getField :: Has '(s,a) rs => proxy '(s,a) -> Rec rs -> a
getField p = aux . get p
  where aux :: Field '(s,a) -> a
        aux (Field x) = x
