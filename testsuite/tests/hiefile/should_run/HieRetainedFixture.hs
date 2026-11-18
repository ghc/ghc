{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures, RankNTypes #-}

module HieRetainedFixture where

data Nat = Z | S Nat
data Proxy (n :: Nat) = Proxy

visible :: (foreach (n :: Nat) -> Proxy n) -> ()
visible _ = ()

invisible :: (foreach (n :: Nat). Proxy n) -> ()
invisible _ = ()

opaque :: (foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Proxy (f n)) -> ()
opaque _ = ()

invisibleIdentity :: foreach (n :: Nat). Proxy n -> Proxy n
invisibleIdentity proxy = proxy

-- | Infer the invisible operand and retain its index in the result type.
useInferred :: foreach (n :: Nat) -> Proxy n -> Proxy n
useInferred n proxy = invisibleIdentity proxy
