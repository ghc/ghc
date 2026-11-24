{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures #-}
{-# LANGUAGE PolyKinds, RankNTypes, ScopedTypeVariables, StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module NativeNonuniformGadtFieldsProvider where

import Data.Kind (Type)
#ifndef LEGACY_ONLY
import Data.Type.Equality ((:~:) (Refl))

data Nat = Zero | Succ Nat

type Fin :: Nat -> Type
data Fin n where
  FZ :: Fin ('Succ n)
  FS :: Fin n -> Fin ('Succ n)

type UTerm :: Nat -> Type
data UTerm n where
  UVar :: Fin n -> UTerm n
  ULam :: UTerm ('Succ n) -> UTerm n
  UApp :: UTerm n -> UTerm n -> UTerm n

type Par :: forall (n :: Nat). UTerm n -> UTerm n -> Type
data Par t t' where
  PVar :: Par ('UVar x) ('UVar x)
  PLam :: Par b b' -> Par ('ULam b) ('ULam b')
  PApp :: Par l l' -> Par m m' -> Par ('UApp l m) ('UApp l' m')

parReflCase :: forall (n :: Nat). foreach (m :: UTerm n) -> Par m m
parReflCase m = case m of
  UVar _   -> PVar
  ULam b   -> PLam (parReflCase b)
  UApp l r -> PApp (parReflCase l) (parReflCase r)

parReflEq :: forall (n :: Nat). foreach (m :: UTerm n) -> Par m m
parReflEq (UVar _)   = PVar
parReflEq (ULam b)   = PLam (parReflEq b)
parReflEq (UApp l r) = PApp (parReflEq l) (parReflEq r)

{-# INLINE inspectOuter #-}
inspectOuter :: foreach (m :: UTerm n) -> Maybe (Par m m)
inspectOuter m = case m of
  UVar _ -> Just PVar
  ULam _ -> Nothing
  UApp _ _ -> Nothing

{-# INLINE diagonal #-}
diagonal :: forall a (x :: a). foreach (p :: x :~: x) -> p :~: 'Refl
diagonal p = case p of Refl -> Refl

#ifdef WRONG_NONUNIFORM
wrong :: forall (n :: Nat). foreach (m :: UTerm n) -> Par m m
wrong m = case m of
  UVar _   -> PVar
  ULam b   -> PLam (parReflCase b)
  UApp l r -> PApp (parReflCase r) (parReflCase l)
#endif
#endif

data Swap (a :: Type) (b :: Type) = Stop | Next (Swap b a)

type SwapShape :: Swap Bool Bool -> Type
data SwapShape s where
  Stopped :: SwapShape 'Stop
  Advanced :: SwapShape ('Next s)

{-# INLINE inspectSwap #-}
inspectSwap :: foreach (s :: Swap Bool Bool) -> SwapShape s
inspectSwap s = case s of
  Stop -> Stopped
  Next _ -> Advanced
