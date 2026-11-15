{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module GroundApplicationClient where

import Data.Proxy (Proxy (Proxy))
import Data.Type.Equality ((:~:) (Refl))
import PackedVecLibrary
import QualifiedPiLibrary

-- | Construct closed nominal evidence under ImpredicativeTypes.
qRefl :: QPi :~: QPi
qRefl = Refl

-- | Supply a bottom proof with a completely known native witness type.
groundBottomProof :: QPi :~: QPi
groundBottomProof = error "native-ground-bottom-proof"

-- | Eliminate a witness without instantiating an outer type variable.
withKnownQ :: (QPi :~: QPi) -> QPi -> QPi
withKnownQ proof f = case proof of Refl -> f

-- | Keep evidence between the two explicit erased type arguments.
withEqQ :: forall tag. (Eq tag) => forall value. (value :~: QPi) -> tag -> value -> QPi
withEqQ proof tag f =
    case tag == tag of
        True -> case proof of Refl -> f
        False -> error "native-ground-invalid-equality"

-- | Apply the witness worker directly at the runtime index.
groundDirect :: foreach (n :: Nat) -> QPi -> Vec Int n
groundDirect n f = withQ @QPi Refl f n

-- | Apply the same worker with the named closed witness.
groundNamed :: foreach (n :: Nat) -> QPi -> Vec Int n
groundNamed n f = withQ @QPi qRefl f n

-- | Check a witness argument against a monomorphic worker domain.
groundKnown :: foreach (n :: Nat) -> QPi -> Vec Int n
groundKnown n f = withKnownQ Refl f n

-- | Apply both visible type arguments around the Eq dictionary.
groundOrdered :: foreach (n :: Nat) -> QPi -> Vec Int n
groundOrdered n f = withEqQ @Int @QPi Refl 37 f n

-- | Discharge the ordinary nominal constraint after explicit instantiation.
groundGiven :: foreach (n :: Nat) -> QPi -> Vec Int n
groundGiven n f = exposeQ @QPi f n

-- | Keep the real transport without replacing it with an eta wrapper.
groundTransport :: QPi -> QPi
groundTransport f = withQ @QPi Refl f

-- | Keep the named witness in the returned-function application.
groundNamedTransport :: QPi -> QPi
groundNamedTransport f = withQ @QPi qRefl f

-- | Demand the supplied proof before exposing the original function.
groundDemandProof :: QPi -> QPi
groundDemandProof f = withQ @QPi groundBottomProof f

-- | Demand the same proof through a monomorphic worker.
groundKnownDemandProof :: QPi -> QPi
groundKnownDemandProof f = withKnownQ groundBottomProof f

-- | Ignore the entire worker application and both bottom arguments.
groundIgnore :: QPi -> Int
groundIgnore f = ignoreQ (withQ @QPi groundBottomProof f)

-- | Return the original function without demanding the ignored witness.
groundIgnoreProof :: QPi -> QPi
groundIgnoreProof f = ignoreProofQ @QPi groundBottomProof f

-- | Leave the proof unforced while the value argument is still absent.
groundPartialProof :: QPi -> QPi
groundPartialProof = withQ @QPi groundBottomProof

-- | Demand the tag through the intervening Eq dictionary.
groundDemandTag :: QPi -> QPi
groundDemandTag f = withEqQ @Int @QPi Refl (error "native-ground-bottom-tag") f

-- | Accept a phantom type argument without adding a value argument.
phantomValue :: forall value. Int
phantomValue = 42

-- | Exercise a native visible argument with an ordinary residual result.
groundPhantom :: Int
groundPhantom = phantomValue @QPi

-- | Supply a fixed ordinary result when the proxy is demanded.
proxyValue :: Proxy QPi -> Int
proxyValue Proxy = 42

-- | Instantiate Proxy's inferred kind binder before its visible argument.
groundProxy :: Int
groundProxy = proxyValue (Proxy @QPi)
