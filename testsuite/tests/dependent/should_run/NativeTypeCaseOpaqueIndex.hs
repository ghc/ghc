{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, TypeFamilies #-}
{-# LANGUAGE PolyKinds, RankNTypes, RequiredTypeArguments, StandaloneKindSignatures #-}
module NativeTypeCaseOpaqueIndex where
import Data.Kind (Type)
import Data.Proxy (Proxy)

type family Ix a

data Gate k where
  Plain :: Bool -> Gate k
  Family :: Proxy a -> Gate (Ix a)

type Bad (d :: Maybe (Gate Ordering)) = case d of
  Just (Plain _) -> Int
  Just _ -> Bool
  Nothing -> Char

type family ClosedIx a where
  ClosedIx Bool = Char
  ClosedIx a = Ordering

data ClosedGate k where
  ClosedPlain :: Bool -> ClosedGate k
  ClosedFamily :: Proxy a -> ClosedGate (ClosedIx a)

type Blocked (d :: Maybe (ClosedGate Ordering)) = case d of
  Just (ClosedPlain _) -> Int
  Just _ -> Bool
  Nothing -> Char

type family ConditionalIx (b :: Bool) where
  ConditionalIx 'True = Bool

data ConditionalGate k (b :: Bool) where
  ConditionalPlain :: Bool -> ConditionalGate k b
  ConditionalFamily :: Bool -> ConditionalGate (ConditionalIx b) b

type NeedsRefinement (d :: Maybe (ConditionalGate Bool 'True)) = case d of
  Just (ConditionalPlain True) -> Int
  Just _ -> Bool
  Nothing -> Char

type family DependentKind where
  DependentKind = Bool

type DependentGate :: forall (k :: Type) -> k -> Type
data DependentGate k value where
  DependentPlain :: Bool -> DependentGate k value
  DependentFamily :: forall (value :: DependentKind).
                     Proxy value -> DependentGate DependentKind value

type NeedsTransport (d :: Maybe (DependentGate Bool 'True)) = case d of
  Just (DependentPlain True) -> Int
  Just _ -> Bool
  Nothing -> Char
