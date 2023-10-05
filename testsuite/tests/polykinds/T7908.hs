{-# LANGUAGE GADTs, InstanceSigs, DataKinds, PolyKinds, RankNTypes, LambdaCase #-}

module T7908 where

import Data.Kind (Type)

class Monad' (m :: (k -> Type) -> Type) where
  return' :: c a -> m c
  (>>>=) :: m c -> (forall a . c a -> m d) -> m d
  (>>-) :: m c -> (forall a . c a -> d) -> d


data Nat = Z' | S' Nat

data Nat' (n :: Nat) where
  Z :: Nat' Z'
  S :: Nat' n -> Nat' (S' n)

data Hidden :: (k -> Type) -> Type where
  Hide :: m a -> Hidden m

instance Monad' Hidden where
  return' :: forall k (c :: k -> Type) (a :: k) . c a -> Hidden c
  return' = Hide
  (>>>=) :: forall k (c :: k -> Type) (d :: k -> Type) .
            Hidden c -> (forall (a :: k) . c a -> Hidden d) -> Hidden d
  Hide a >>>= f = f a
  (>>-) :: forall k (c :: k -> Type) d .
           Hidden c -> (forall (a :: k) . c a -> d) -> d
  Hide a >>- f = f a


int2nat' 0 = return' Z
int2nat' i = (int2nat' $ i - 1) >>>= (\n -> return' $ S n)


data Fin (m :: Nat)  (n :: Nat) where
  Fz :: Fin (S' m) Z'
  Fs :: Fin m n -> Fin (S' m) (S' n)

-- N.B. not total!
nat2fin :: Nat' f -> Hidden Nat' -> Hidden (Fin f)
nat2fin (S _) (Hide Z) = return' Fz
nat2fin (S f) n = n >>>= (\case S n -> (nat2fin f (return' n) >>>= (\fn -> return' $ Fs fn)))

fin2int :: Hidden (Fin f) -> Int
fin2int f = f >>- go
  where go :: Fin f n -> Int
        go Fz = 0
        go (Fs f) = 1 + go f


test = fin2int (nat2fin (S $ S Z) $ return' (S Z))
