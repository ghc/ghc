{-# LANGUAGE GHC2024, ExperimentalDependentTypes #-}
module Main where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl), (:~~:)(HRefl), castWith)
import Numeric.Natural (Natural)

-- | An ordinary index whose parameter has a polymorphic kind.
data Ix (a :: k) = Ix Natural

-- | A result indexed by the retained argument.
data Out :: forall k (a :: k). Ix a -> Type where
    Out :: Natural -> Out x

-- | Read the carried value.
getIx :: Ix a -> Natural
getIx (Ix n) = n

-- | Build the indexed result from the original argument.
at :: forall k (a :: k). foreach (x :: Ix a) -> Out x
at x = Out (getIx x)

-- | Refine the kind before binding the retained argument.
before :: forall k l (a :: k) (b :: l).
    a :~~: b -> (foreach (x :: Ix b) -> Out x) ->
    foreach (x :: Ix a) -> Out x
before HRefl f x = f x

-- | Refine the kind after binding the retained argument.
after :: forall k l (a :: k) (b :: l).
    (foreach (x :: Ix b) -> Out x) -> foreach (x :: Ix a) -> a :~~: b -> Out x
after f x proof = case proof of HRefl -> f x

-- | Return the retained function directly after refining the kind.
whole :: forall k l (a :: k) (b :: l).
    a :~~: b -> (foreach (x :: Ix b) -> Out x) ->
    foreach (x :: Ix a) -> Out x
whole HRefl f = f

-- | Package a function at one chosen index kind.
data F :: forall k. k -> Type where
    F :: (foreach (x :: Ix a) -> Out x) -> F a

-- | Transport the package using homogeneous equality.
usingCastWith :: forall k l (a :: k) (b :: l).
    F b :~: F a -> F b -> foreach (x :: Ix a) -> Out x
usingCastWith proof f x = case castWith proof f of F go -> go x

-- | Read the result while preserving its runtime index.
observe :: forall k (a :: k). foreach (x :: Ix a) -> Out x -> Natural
observe _ (Out n) = n

main :: IO ()
main =
    let v1, v2, v3, v4 :: Ix 'True
        v1 = Ix 41
        v2 = Ix 42
        v3 = Ix 43
        v4 = Ix 44
        readOut :: foreach (x :: Ix 'True) -> Out x -> Natural
        readOut = observe @Bool @'True
    in print ( readOut v1 (before @Bool @Bool @'True @'True HRefl (at @Bool @'True) v1)
             , readOut v2 (after @Bool @Bool @'True @'True (at @Bool @'True) v2 HRefl)
             , readOut v3 (whole @Bool @Bool @'True @'True HRefl (at @Bool @'True) v3)
             , readOut v4 (usingCastWith @Bool @Bool @'True @'True Refl (F (at @Bool @'True)) v4)
             )
