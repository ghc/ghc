{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeFamilies, BangPatterns, TypeOperators #-}
module NativeSimplifierProvider (choose, keep, Tagged(..), keepTagged, one, toList, typeOnly, Nat(..), observeLengths, observeDemand, lazyLengths, lazyPartial, impossible, scopeAnnotations, Packed, sample, observePacked, driveSame, scopeLet, scopeTop, scopeKnown) where

import Data.IORef (newIORef, readIORef)
import Data.Type.Equality ((:~:)(Refl))

data Nat = Z | S Nat
data Vec a (n :: Nat) where
  Nil :: Vec a 'Z
  Cons :: a -> Vec a n -> Vec a ('S n)

type family Length (b :: Bool) :: Nat where
  Length 'False = 'Z
  Length 'True = 'S 'Z

choose :: foreach (b :: Bool) -> Vec Int (Length b)
choose False = Nil
choose True = Cons 7 Nil

keep :: foreach (b :: Bool) -> Vec Int (Length b) -> Vec Int (Length b)
keep b xs = xs

toList :: Vec a n -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

{-# NOINLINE opaque #-}
opaque :: Bool -> Bool
opaque b = not b

{-# NOINLINE consume #-}
consume :: Maybe a -> Int
consume _ = 19

typeOnly :: Bool -> Int
typeOnly source =
  let b :: Bool
      b = opaque source
  in consume @(Vec Int (Length b)) Nothing

data Tagged (b :: Bool) = Tagged (Vec Int ('S 'Z))

keepTagged :: foreach (b :: Bool) -> Tagged b -> Tagged b
keepTagged b xs = xs

one :: Vec Int ('S 'Z)
one = Cons 7 Nil

nested :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> Vec Int m -> ([Int], [Int])
nested n m xs ys = (toList xs, toList ys)

nestedDemand :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> Vec Int m -> ([Int], [Int])
nestedDemand n m xs ys =
  case n of
    Z -> case m of
      Z -> ([], [])
      S _ -> ([], toList ys)
    S _ -> case m of
      Z -> (toList xs, [])
      S _ -> (toList xs, toList ys)

impossible :: forall a. Vec a 'Z -> Int
impossible Nil = 37

fromNat :: foreach (n :: Nat) -> Int -> Vec Int n
fromNat Z x = Nil
fromNat (S k) x = Cons x (fromNat k x)

observeLengths :: Nat -> Nat -> ([Int], [Int])
observeLengths n m = nested n m (fromNat n 3) (fromNat m 5)

observeDemand :: Nat -> Nat -> ([Int], [Int])
observeDemand n m = nestedDemand n m (fromNat n 3) (fromNat m 5)

lazyLengths :: Nat -> Nat -> Bool
lazyLengths n m = nested n m (fromNat n 3) (fromNat m 5) `seq` True

lazyPartial :: foreach (n :: Nat) -> Bool
lazyPartial n = case nested n of { !_ -> True }

type Ignored (b :: Bool) = Bool

scopeAnnotations :: Bool -> Int
scopeAnnotations source =
  let b :: Bool
      b = opaque source
  in
  let
      keep :: Ignored b
      keep = True
      recurse :: Ignored b -> Int
      recurse x = if x then 20 else recurse True
  in case (if keep then False else True :: forall (index :: Bool). Ignored index) of
       False -> recurse False
       True -> 21

data Packed where
  Pack :: foreach (n :: Nat) -> Vec Int n -> Packed

sample :: Packed
sample = Pack (S (S Z)) (Cons 7 (Cons 8 Nil))

observePacked :: Packed -> [Int]
observePacked (Pack _ xs) = toList xs

data Same where
  Same :: foreach (x :: Bool) -> foreach (y :: Bool) -> (x :~: y) -> Same

buildSame :: Bool -> Same
buildSame x = Same x x Refl

{-# NOINLINE consumeSame #-}
consumeSame :: Same -> Bool
consumeSame (Same _ _ Refl) = True

{-# NOINLINE driveSame #-}
driveSame :: Bool -> IO Bool
driveSame input = do
  cell <- newIORef input
  x <- readIORef cell
  pure (consumeSame (buildSame x))

{-# NOINLINE observeIndexed #-}
observeIndexed :: Vec Int n -> [Int]
observeIndexed = toList

scopeLet :: [Int]
scopeLet =
  let source :: Bool
      source = True
  in observeIndexed (choose source)

sharedIndex :: Bool
sharedIndex = True

scopeTop :: [Int]
scopeTop = observeIndexed (choose sharedIndex)

{-# NOINLINE observeProof #-}
observeProof :: ((p :: Bool) :~: (q :: Bool)) -> Int
observeProof Refl = 8

scopeKnown :: Bool -> Int
scopeKnown b = case buildSame b of
  Same p q proof -> observeProof proof
