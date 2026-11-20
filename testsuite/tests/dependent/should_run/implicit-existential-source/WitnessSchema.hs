{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ImpredicativeTypes, LinearTypes, PolyKinds, RankNTypes, TypeApplications #-}
module WitnessSchema
    ( Hidden, callback, unpack, Multi, pair, kindBox, KindBox, Deep, producer
    , CallbackPair, atTwoTypes, observePair, ignorePair
    , RankedBuilder, buildAt, configured, observeBuilder
    , Flag(..), FlaggedBuilder, observeFlagged
    , Captured(..), CapturedBuilder, LinearBuilder, buildLinear, buildTwice
    ) where
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

type CallbackPair = exists (t :: Type). (t, t -> (Int, Bool))
atTwoTypes :: PolyId -> (Int, Bool)
atTwoTypes f = (f 7, f True)

observePair :: CallbackPair -> (Int, Bool)
observePair (f, k) = k f

ignorePair :: CallbackPair -> Int
ignorePair (_, _) = 9

type RankedBuilder = exists (t :: Type). Bool -> Payload t

buildAt :: Bool -> Payload PolyId
buildAt b = Payload @PolyId id (\f -> (f (if b then 7 else 9), f b))

configured :: forall {k} (flag :: k) tag. Proxy flag -> (tag -> Bool) -> tag -> Payload PolyId
configured _ test tag = buildAt (test tag)

observeBuilder :: RankedBuilder -> Bool -> (Int, Bool)
observeBuilder builder b = observePayload (builder b)

data Flag a where
    Flag :: Flag Bool

type FlaggedBuilder a = exists (t :: Type). Flag a -> a -> Payload t

observeFlagged :: FlaggedBuilder Bool -> Bool -> (Int, Bool)
observeFlagged builder b = observePayload (builder Flag b)

observePayload :: Payload t -> (Int, Bool)
observePayload (Payload value run) = run value

data Captured where
    Captured :: Payload (forall r. r -> x) -> Captured

type CapturedBuilder = exists (t :: Type). Captured -> Payload t
type LinearBuilder = exists (t :: Type). Bool %1 -> Payload t

buildLinear :: Bool %1 -> Payload PolyId
buildLinear b = case b of
    True  -> buildAt True
    False -> buildAt False

buildTwice :: Bool %1 -> Bool %1 -> Payload PolyId
buildTwice left right = case left of
    True  -> buildLinear right
    False -> buildLinear right
