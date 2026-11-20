{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, RankNTypes, UndecidableInstances #-}
module NativeGadtRetainedSumProvider where

import GHC.Sigma (Sigma(Pair))

data Nat = Z | S Nat

data Vec a (n :: Nat) where
    VNil :: Vec a Z
    VCons :: a -> Vec a n -> Vec a (S n)

cells :: Vec a n -> [a]
cells VNil = []
cells (VCons x xs) = x : cells xs

lengthValue :: Nat -> Int
lengthValue Z = 0
lengthValue (S n) = 1 + lengthValue n

zipVec :: Vec a n -> Vec b n -> Vec (a, b) n
zipVec VNil VNil = VNil
zipVec (VCons x xs) (VCons y ys) = VCons (x, y) (zipVec xs ys)

data Nested a where
    Nested :: (some (n :: Nat) | (Vec a n, (some (m :: Nat) | (Vec a n, Vec a m)))) -> Nested a

makeNested :: forall a. foreach (n :: Nat) -> foreach (m :: Nat) -> Vec a n -> Vec a n -> Vec a m -> Nested a
makeNested n m xs ys zs = Nested (Pair n (xs, Pair m (ys, zs)))

data Hidden a where
    Hidden :: forall a (n :: Nat). Vec a n -> (some (m :: Nat) | (Vec a n, Vec a m)) -> Hidden a

makeHidden :: forall a (n :: Nat). foreach (m :: Nat) -> Vec a n -> Vec a n -> Vec a m -> Hidden a
makeHidden m xs ys zs = Hidden @a @n xs (Pair m (ys, zs))

withHidden :: Hidden a -> (forall (n :: Nat). foreach (m :: Nat) -> Vec a n -> Vec a n -> Vec a m -> r) -> r
withHidden (Hidden xs (Pair m (ys, zs))) k = k m xs ys zs

storedLengths :: Nested a -> (Int, Int)
storedLengths (Nested (Pair n (_, Pair m _))) = (lengthValue n, lengthValue m)

innerCells :: Nested a -> [a]
innerCells (Nested (Pair _ (_, Pair _ (_, zs)))) = cells zs

outerWitnessBottom :: Nested Bool
outerWitnessBottom = build (error "outer witness")
  where
    build :: Nat -> Nested Bool
    build n = makeNested n (S Z) (error "first vector")
        (error "companion vector") (VCons True VNil)

innerWitnessBottom :: Nested Bool
innerWitnessBottom = build (error "inner witness")
  where
    build :: Nat -> Nested Bool
    build m = makeNested (S Z) m (VCons True VNil)
        (VCons False VNil) (error "second vector")

innerPairBottom :: Nested Bool
innerPairBottom = Nested (Pair (S Z) (VCons True VNil, error "inner pair"))
