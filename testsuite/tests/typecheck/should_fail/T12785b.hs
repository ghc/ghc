{-# LANGUAGE RankNTypes, TypeOperators, ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds, GADTs #-}

module T12785b where

import Data.Kind

data Peano = Z | S Peano

data HTree n a where
  Point :: a -> HTree Z a
  Leaf :: HTree (S n) a
  Branch :: a -> HTree n (HTree (S n) a) -> HTree (S n) a

data STree (n ::Peano) :: forall a . (a -> Type) -> HTree n a -> Type where
  SPoint :: f a -> STree Z f (Point a)
  SLeaf :: STree (S n) f Leaf
  SBranch :: f a -> STree n (STree (S n) f) stru -> STree (S n) f (a `Branch` stru)
  SBranchX :: (Payload (S n) (Payload n stru) ~ a)
          => f a -> STree n (STree (S n) f) stru -> STree (S n) f (a `Branch` stru)

data Hidden :: Peano -> (a -> Type) -> Type where
  Hide :: STree n f s -> Hidden n f

nest :: HTree m (Hidden (S m) f) -> Hidden m (STree ('S m) f)
nest (Point (Hide st)) = Hide (SPoint st)
nest Leaf = Hide SLeaf
nest (Hide a `Branch` (nest . hmap nest -> Hide tr)) = Hide $ a `SBranchX` tr

hmap :: (x -> y) -> HTree n x -> HTree n y
hmap f (Point a) = Point (f a)
hmap f Leaf = Leaf
hmap f (a `Branch` tr) = f a `Branch` hmap (hmap f) tr

type family Payload (n :: Peano) (s :: HTree n x) :: x where
  Payload Z (Point a) = a
  Payload (S n) (a `Branch` stru) = a
