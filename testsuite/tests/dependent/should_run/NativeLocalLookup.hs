{-# LANGUAGE CPP, DataKinds, EmptyCase, ExperimentalDependentTypes, GADTs #-}
{-# LANGUAGE PolyKinds, RankNTypes, StandaloneKindSignatures, TypeOperators #-}
#ifdef SPLICE_UNUSED_ABSTRACT
{-# LANGUAGE TemplateHaskell #-}
#endif
#if defined(HIDE_ENV_CONS)
module NativeLocalLookup (Nat(..), Fin(..), UTerm(..), Env, Clos(..), lookupEnv, EvalBig(..)) where
#elif defined(HIDE_FIN_CONS)
module NativeLocalLookup (Nat(..), Fin, UTerm(..), Env(..), Clos(..), lookupEnv, EvalBig(..)) where
#elif defined(HIDE_CLOS_CONS)
module NativeLocalLookup (Nat(..), Fin(..), UTerm(..), Env(..), Clos, lookupEnv, EvalBig(..)) where
#elif defined(HIDE_LOOKUP)
module NativeLocalLookup (Nat(..), Fin(..), UTerm(..), Env(..), Clos(..), EvalBig(..)) where
#elif defined(ABSTRACT_EVAL)
module NativeLocalLookup
  ( Nat(..), Fin(..), UTerm(..), Env(..), Clos(..), lookupEnv
  , EvalBig, IsLamClos(..), evalBigIsLam, evalIdApp, evalKIdOmega ) where
#elif defined(SPLICE_UNUSED_ABSTRACT)
module NativeLocalLookup (Nat(..), Fin(..), UTerm(..), Env, Clos(..), lookupEnv, sampleClos) where
#elif defined(SINGLE_PRODUCT_CLOS)
module NativeLocalLookup (Nat(..), Fin(..), UTerm(..), Env(..), Clos(..), lookupEnv, EvalBig(..), firstVar) where
#else
module NativeLocalLookup
  ( Nat(..), Fin(..), UTerm(..), Env(..), Clos(..), lookupEnv
  , EvalBig(..), FunctionalVar(..), IsLamClos(..), evalBigIsLam
  , evalDeterm, idClos, evalIdApp, evalConstOmega, evalKIdOmega
  , firstVar, nextVar ) where
#endif

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))
#ifdef IMPORTED_ORPHAN_RULE
import OrphanLookupRule ()
#endif

#ifdef REVERSE_DECLS
#ifndef SPLICE_UNUSED_ABSTRACT
type EvalBig :: forall (n :: Nat). Env n -> UTerm n -> Clos -> Type
data EvalBig e t v where
  EVar :: (lookupEnv e x ~ 'MkClos m d) => EvalBig d m v -> EvalBig e ('UVar x) v
  ELam :: EvalBig e ('ULam b) ('MkClos ('ULam b) e)
  EApp :: EvalBig e l ('MkClos ('ULam b) d)
       -> EvalBig ('ECons ('MkClos m e) d) b w
       -> EvalBig e ('UApp l m) w
#endif

#ifdef EXPLICIT_FORALL
lookupEnv :: forall (n :: Nat). Env n -> Fin n -> Clos
#else
lookupEnv :: Env n -> Fin n -> Clos
#endif
#ifdef WRONG_BODY
lookupEnv (ECons c _) FZ     = c
lookupEnv (ECons c _) (FS _) = c
#elif defined(REVERSE_CLAUSES)
lookupEnv (ECons _ e) (FS x) = lookupEnv e x
lookupEnv (ECons c _) FZ     = c
#else
lookupEnv (ECons c _) FZ     = c
lookupEnv (ECons _ e) (FS x) = lookupEnv e x
#endif
#endif

data Nat = Zero | Succ Nat

data Fin (n :: Nat) where
  FZ :: Fin ('Succ n)
  FS :: Fin n -> Fin ('Succ n)

data UTerm (n :: Nat) where
  UVar :: Fin n -> UTerm n
  ULam :: UTerm ('Succ n) -> UTerm n
  UApp :: UTerm n -> UTerm n -> UTerm n

data Env (n :: Nat) where
  ENil  :: Env 'Zero
  ECons :: Clos -> Env n -> Env ('Succ n)

#ifdef SINGLE_PRODUCT_CLOS
data Clos = MkClos Bool
#else
data Clos where
  MkClos :: UTerm n -> Env n -> Clos
#endif

#ifndef REVERSE_DECLS
#ifdef EXPLICIT_FORALL
lookupEnv :: forall (n :: Nat). Env n -> Fin n -> Clos
#else
lookupEnv :: Env n -> Fin n -> Clos
#endif
#ifdef WRONG_BODY
lookupEnv (ECons c _) FZ     = c
lookupEnv (ECons c _) (FS _) = c
#elif defined(REVERSE_CLAUSES)
lookupEnv (ECons _ e) (FS x) = lookupEnv e x
lookupEnv (ECons c _) FZ     = c
#else
lookupEnv (ECons c _) FZ     = c
lookupEnv (ECons _ e) (FS x) = lookupEnv e x
#endif

#ifndef SPLICE_UNUSED_ABSTRACT
#ifdef SINGLE_PRODUCT_CLOS
type EvalBig :: forall (n :: Nat). Env n -> UTerm n -> Clos -> Type
data EvalBig e t v where
  EVar :: EvalBig e ('UVar x) (lookupEnv e x)
#else
type EvalBig :: forall (n :: Nat). Env n -> UTerm n -> Clos -> Type
data EvalBig e t v where
  EVar :: (lookupEnv e x ~ 'MkClos m d) => EvalBig d m v -> EvalBig e ('UVar x) v
  ELam :: EvalBig e ('ULam b) ('MkClos ('ULam b) e)
  EApp :: EvalBig e l ('MkClos ('ULam b) d)
       -> EvalBig ('ECons ('MkClos m e) d) b w
       -> EvalBig e ('UApp l m) w
#endif
#endif
#endif

#ifdef SPECIALISE_LOOKUP
{-# SPECIALISE lookupEnv :: Env ('Succ 'Zero) -> Fin ('Succ 'Zero) -> Clos #-}
#endif

#ifdef SPLICE_UNUSED_ABSTRACT
$(pure [])

sampleClos :: Clos
sampleClos = lookupEnv (ECons (MkClos (ULam (UVar FZ)) ENil) ENil) FZ
#elif defined(SINGLE_PRODUCT_CLOS)
firstVar :: forall n (c :: Clos) (e :: Env n). EvalBig ('ECons c e) ('UVar 'FZ) c
firstVar = EVar
#else

type IdTerm = 'ULam ('UVar 'FZ)
type OmegaTerm = 'UApp ('ULam ('UApp ('UVar 'FZ) ('UVar 'FZ)))
                       ('ULam ('UApp ('UVar 'FZ) ('UVar 'FZ)))

idClos :: Clos
idClos = MkClos (ULam (UVar FZ)) ENil

evalIdApp :: EvalBig 'ENil ('UApp IdTerm IdTerm) ('MkClos IdTerm 'ENil)
evalIdApp = EApp ELam (EVar ELam)

evalConstOmega
  :: EvalBig 'ENil
       ('UApp ('ULam ('ULam ('UVar ('FS 'FZ)))) OmegaTerm)
       ('MkClos ('ULam ('UVar ('FS 'FZ))) ('ECons ('MkClos OmegaTerm 'ENil) 'ENil))
evalConstOmega = EApp ELam ELam

evalKIdOmega
  :: EvalBig 'ENil
       ('UApp ('UApp ('ULam ('ULam ('UVar ('FS 'FZ)))) IdTerm) OmegaTerm)
       ('MkClos IdTerm 'ENil)
evalKIdOmega = EApp (EApp ELam ELam) (EVar ELam)

type FunctionalVar :: forall (n :: Nat). Env n -> UTerm n -> Clos -> Type
data FunctionalVar e t v where
  FunctionalVar :: (lookupEnv e x ~ 'MkClos m d) => EvalBig d m v -> FunctionalVar e ('UVar x) v
  DirectVar :: FunctionalVar e ('UVar x) (lookupEnv e x)

#ifdef WRONG_INDEX
firstVar :: forall n (c :: Clos) (e :: Env n). FunctionalVar ('ECons c e) ('UVar 'FZ) ('MkClos IdTerm 'ENil)
#else
firstVar :: forall n (c :: Clos) (e :: Env n). FunctionalVar ('ECons c e) ('UVar 'FZ) c
#endif
firstVar = DirectVar

nextVar :: forall n (c :: Clos) (e :: Env n) (x :: Fin n).
           FunctionalVar ('ECons c e) ('UVar ('FS x)) (lookupEnv e x)
nextVar = DirectVar

data IsLamClos (c :: Clos) where
  MkIsLamClos :: IsLamClos ('MkClos ('ULam b) e)

evalBigIsLam :: EvalBig e m c -> IsLamClos c
evalBigIsLam (EVar ev)    = evalBigIsLam ev
evalBigIsLam ELam         = MkIsLamClos
evalBigIsLam (EApp _ ev2) = evalBigIsLam ev2

evalDeterm :: EvalBig e m c1 -> EvalBig e m c2 -> c1 :~: c2
evalDeterm (EVar ev1) (EVar ev2) = evalDeterm ev1 ev2
evalDeterm ELam ELam = Refl
evalDeterm (EApp evL1 evB1) (EApp evL2 evB2) = case evalDeterm evL1 evL2 of
  Refl -> evalDeterm evB1 evB2

#ifdef LOCAL_RULE
{-# NOINLINE lookupEnv #-}
{-# RULES "changed local lookup" forall c e. lookupEnv (ECons c e) FZ = MkClos (ULam (UVar FZ)) ENil #-}
#endif
#endif
