{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, StandaloneKindSignatures #-}
-- | The congruence fragment of parallel reduction in PLFH Confluence.
-- Adapted from PLFH under CC BY 4.0. See REFERENCES.md.
module BookParallel (Fin(..), UTerm(..), Par(..), parRefl) where

import Data.Kind (Type)
import HaskLib.Nat (Nat(..))

-- | A variable in a context with the given number of entries.
data Fin (n :: Nat) where
  FZ :: Fin (Succ n)
  FS :: Fin n -> Fin (Succ n)

-- | An untyped lambda term. A lambda adds one variable to the context.
data UTerm (n :: Nat) where
  UVar :: Fin n -> UTerm n
  ULam :: UTerm (Succ n) -> UTerm n
  UApp :: UTerm n -> UTerm n -> UTerm n

-- | The variable, lambda and application cases of parallel reduction.
type Par :: forall (n :: Nat). UTerm n -> UTerm n -> Type
data Par t t' where
  PVar :: Par (UVar x) (UVar x)
  PLam :: Par b b' -> Par (ULam b) (ULam b')
  PApp :: Par l l' -> Par m m' -> Par (UApp l m) (UApp l' m')

-- | Every term can reduce to itself without contracting a redex.
parRefl :: forall (n :: Nat). foreach (m :: UTerm n) -> Par m m
parRefl (UVar _)   = PVar
parRefl (ULam b)   = PLam (parRefl b)
parRefl (UApp l m) = PApp (parRefl l) (parRefl m)
