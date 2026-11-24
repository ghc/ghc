{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, StandaloneKindSignatures #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}
-- | Environment lookup from the PLFH BigStep chapter. Adapted under CC BY 4.0.
module BookEnvironment (Env(..), Clos(..), EnvLookup(..), EvalBig(..),
                        lookupEnv, secondIdentity, evalSecondIdentity) where

import BookParallel (Fin(..), UTerm(..))
import Data.Kind (Type)
import HaskLib.Nat (Nat(..))

-- | A closure pairs a term with the environment for its free variables.
data Clos where
    MkClos :: UTerm n -> Env n -> Clos

-- | An environment contains one closure for each variable in scope.
data Env (n :: Nat) where
    ENil  :: Env Zero
    ECons :: Clos -> Env n -> Env (Succ n)

-- | Look up a variable by its distance from the most recent binding.
lookupEnv :: Env n -> Fin n -> Clos
lookupEnv (ECons c _) FZ     = c
lookupEnv (ECons _ e) (FS x) = lookupEnv e x

-- | Evidence that a particular environment position contains the given closure.
type EnvLookup :: forall n. Env n -> Fin n -> Clos -> Type
data EnvLookup e x c where
    ELZ :: EnvLookup (ECons c e) FZ c
    ELS :: EnvLookup e x c -> EnvLookup (ECons other e) (FS x) c

type Identity = MkClos (ULam (UVar FZ)) ENil

-- | The identity function is one binding below a different closure.
secondIdentity :: EnvLookup (ECons (MkClos (ULam (ULam (UVar FZ))) ENil)
                                   (ECons Identity ENil)) (FS FZ) Identity
secondIdentity = ELS ELZ

-- | Big-step evaluation of a variable looks up its closure in the same module.
type EvalBig :: forall n. Env n -> UTerm n -> Clos -> Type
data EvalBig e t c where
    EVar :: EvalBig e (UVar x) (lookupEnv e x)
    ELam :: EvalBig e (ULam b) (MkClos (ULam b) e)

-- | Evaluate the second variable in the same module where lookupEnv is defined.
evalSecondIdentity :: EvalBig (ECons (MkClos (ULam (ULam (UVar FZ))) ENil)
                                     (ECons Identity ENil)) (UVar (FS FZ)) Identity
evalSecondIdentity = EVar
