{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ImpredicativeTypes, PolyKinds, RankNTypes, TypeApplications #-}
module WitnessSchema (Hidden, callback, unpack, Multi, pair, kindBox, KindBox, Deep, producer) where
import Data.Kind (Type)
import Data.Proxy

type PolyId = forall a. a -> a
type PolyMaybe = forall a. a -> Maybe a
data Payload t = Payload t (t -> (Int,Bool))
type Hidden = exists (t :: Type). Payload t
callback :: Payload PolyId
callback = Payload @PolyId id (\f -> (f 7, f True))
unpack :: Hidden -> (Int, Bool)
unpack p = case p of Payload x observe -> observe x

data Pair x y = Pair x y
type Multi = exists (x :: Type). exists (y :: Type). Pair x y
pair :: Pair PolyId PolyMaybe
pair = Pair @PolyId @PolyMaybe id Just

type KindBox = exists (k :: Type). exists (t :: k). Proxy t
kindBox :: Proxy PolyId
kindBox = Proxy

type Deep = exists (t :: Type). (t -> Bool) -> t

producer :: ((forall x. x) -> Bool) -> Int
producer _ = 7
