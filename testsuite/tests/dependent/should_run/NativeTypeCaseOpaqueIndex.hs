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
  Just (Family _) -> Int
  Just _ -> Bool
  Nothing -> Char

type family ClosedIx a where
  ClosedIx Bool = Char
  ClosedIx a = Ordering

data ClosedGate k where
  ClosedPlain :: Bool -> ClosedGate k
  ClosedFamily :: Proxy a -> ClosedGate (ClosedIx a)

type Blocked (d :: Maybe (ClosedGate Ordering)) = case d of
  Just (ClosedFamily _) -> Int
  Just _ -> Bool
  Nothing -> Char

type family ConditionalIx (b :: Bool) where
  ConditionalIx 'True = Bool
type family NonInjectiveIx (b :: Bool) where
  NonInjectiveIx b = Bool
data ConditionalGate k marker where
  ConditionalPlain :: Bool -> ConditionalGate Bool Bool
  ConditionalFamily :: Proxy b -> ConditionalGate (ConditionalIx b) (NonInjectiveIx b)

type NeedsRefinement (d :: Maybe (ConditionalGate Bool (NonInjectiveIx 'True))) = case d of
  Just (ConditionalPlain True) -> Int
  Just _ -> Bool
  Nothing -> Char
