{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, RankNTypes, TypeOperators, UndecidableInstances #-}
module NativeGadtRetainedSumProvider where

import Data.Type.Equality ((:~:)(Refl))
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

-- | Share the first constructor argument with a later retained sum.
data Later a b where
    Later :: foreach (n :: Nat) -> (some (m :: Nat) | (Vec a n, Vec b m)) -> Later a b

-- | Construct the sum at the two supplied lengths.
makeLater :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec a n -> Vec b m -> Later a b
makeLater n m xs ys = Later n (Pair m (xs, ys))

-- | Give the consumer both lengths and their vectors.
withLater :: Later a b -> (foreach (n :: Nat) -> foreach (m :: Nat) -> Vec a n -> Vec b m -> r) -> r
withLater (Later n (Pair m (xs, ys))) consume = consume n m xs ys

-- | Supply an inner vector with an unused outer witness and vector.
laterWitnessBottom :: Later Bool Char
laterWitnessBottom = build (error "earlier witness")
  where
    build :: Nat -> Later Bool Char
    build n = makeLater n (S Z) (error "earlier vector") (VCons 'x' VNil)

-- | Use a captured value at its declared kind.
data KindTag (a :: k) = KindTag

-- | Use a written index in the domain of a retained flag.
type FlagAt (n :: Nat) = Bool

-- | The function field shadows the length seen by the following field.
data FieldScope (a :: k) where
    FieldScope :: forall k (a :: k). foreach (n :: Nat)
               -> (foreach (n :: Nat) (p :: FlagAt n) -> Vec Bool n
                   -> (some (m :: Nat) | (KindTag a, KindTag p, Vec Bool n, Vec Char m)))
               -> (some (m :: Nat) | (Vec Bool n, Vec Char m))
               -> FieldScope a

-- | Give the two fields different vector lengths.
fieldScope :: FieldScope 'True
fieldScope = FieldScope (S (S Z)) (\_ _ xs -> Pair Z (KindTag, KindTag, xs, VNil))
             (Pair Z (VCons False (VCons True VNil), VNil))

-- | Return a reflexive witness only for the empty vector.
emptyWitness :: forall a n. foreach (xs :: Vec a n) -> Maybe (xs :~: xs)
emptyWitness xs
    | null (cells xs) = Just Refl
    | otherwise = Nothing

-- | Package a vector with its runtime length.
type Packed a = some (n :: Nat) | Vec a n

pack :: foreach (n :: Nat) -> Vec Bool n -> Packed Bool
pack n xs = Pair n xs

-- | Store a vector at the length supplied by the first field.
data DirectVec a (n :: Nat) where
    DirectVec :: foreach (n :: Nat) -> Vec a n -> DirectVec a n

-- | Store a rectangular matrix and both of its dimensions.
data Matrix a (rows :: Nat) (columns :: Nat) where
    Matrix :: foreach (rows :: Nat) (columns :: Nat)
           -> Vec (Vec a columns) rows -> Matrix a rows columns
    MatrixByColumns :: foreach (columns :: Nat) (rows :: Nat)
                    -> Vec (Vec a columns) rows -> Matrix a rows columns

-- | Store a flag and a row length in the opposite order from the result.
data MarkedRows a (rows :: Nat) (marked :: Bool) where
    MarkedRows :: foreach (marked :: Bool) (rows :: Nat)
               -> Vec a rows -> MarkedRows a rows marked

-- | Store a value at any length.
data AtLength (n :: Nat) = AtLength Int

-- | Store a payload whose type uses the length without demanding it.
data DirectAtLength (n :: Nat) where
    DirectAtLength :: foreach (n :: Nat) -> AtLength n -> DirectAtLength n

-- | Store an unrelated runtime witness beside an empty vector.
type Phantom = some (n :: Nat) | Vec Bool Z

phantom :: Phantom
phantom = Pair (S Z) VNil

-- | Compare values whose vector indices must agree.
sameLength :: forall n. foreach (xs :: Vec Bool n) -> foreach (ys :: Vec Bool n) -> Bool
sameLength _ _ = True

-- | Pass the vector through at its original index.
forward :: forall a n. foreach (xs :: Vec a n) -> Vec a n
forward xs = xs
