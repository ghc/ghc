{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, TypeFamilies #-}
module NativeTypeCaseOpaqueIndex where
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
