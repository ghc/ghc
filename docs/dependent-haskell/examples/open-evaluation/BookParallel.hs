{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LambdaCase, PolyKinds, StandaloneKindSignatures #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}
-- | Untyped terms, substitution and congruence of parallel reduction in PLFH Confluence.
-- Adapted from PLFH under CC BY 4.0. See REFERENCES.md.
module BookParallel (Fin(..), UTerm(..), Par(..), parRefl,
                     extU, renameU, extsU, substU, substSingleU) where

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

-- | Extend an untyped variable renaming under a binder.
extU :: (Fin m -> Fin n) -> Fin (Succ m) -> Fin (Succ n)
extU _   FZ     = FZ
extU rho (FS x) = FS (rho x)

-- | Simultaneous renaming on scope-indexed untyped terms.
renameU :: (Fin m -> Fin n) -> UTerm m -> UTerm n
renameU rho (UVar x)   = UVar (rho x)
renameU rho (ULam b)   = ULam (renameU (extU rho) b)
renameU rho (UApp l m) = UApp (renameU rho l) (renameU rho m)

-- | Extend an untyped substitution under a binder.
extsU :: (Fin m -> UTerm n) -> Fin (Succ m) -> UTerm (Succ n)
extsU _     FZ     = UVar FZ
extsU sigma (FS x) = renameU FS (sigma x)

-- | Simultaneous substitution on scope-indexed untyped terms.
substU :: (Fin m -> UTerm n) -> UTerm m -> UTerm n
substU sigma (UVar x)   = sigma x
substU sigma (ULam b)   = ULam (substU (extsU sigma) b)
substU sigma (UApp l m) = UApp (substU sigma l) (substU sigma m)

-- | Substitute a single untyped term for the most recent variable.
substSingleU :: UTerm (Succ n) -> UTerm n -> UTerm n
substSingleU b w = substU (\case { FZ -> w; FS x -> UVar x }) b
