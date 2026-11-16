{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module KnownArgumentClient where

import Data.Type.Equality ((:~:) (Refl))
import GroundApplicationClient (qRefl, withEqQ)
import PackedVecLibrary
import QualifiedPiLibrary

-- | Ignore a value through an ordinary erased type parameter.
ordinaryIgnore :: forall value. value -> Int
ordinaryIgnore _ = 42

-- | Select the erased choice from f before checking Refl.
knownDirect :: foreach (n :: Nat) -> QPi -> Vec Int n
knownDirect n f = withQ Refl f n

-- | Use the named witness without a visible type argument.
knownNamed :: foreach (n :: Nat) -> QPi -> Vec Int n
knownNamed n f = withQ qRefl f n

-- | Leave the value quantifier after the checked tag and Eq evidence.
knownOrdered :: foreach (n :: Nat) -> Int -> QPi -> Vec Int n
knownOrdered n tag f = withEqQ @Int Refl tag f n

-- | Keep both generic choices fixed by declared argument types.
knownConstant :: foreach (n :: Nat) -> Int -> QPi -> Vec Int n
knownConstant n tag f = const f tag n

-- | Retain nominal entailment after implicit type selection.
knownGiven :: foreach (n :: Nat) -> QPi -> Vec Int n
knownGiven n f = exposeQ f n

-- | Instantiate an ordinary generic consumer from the bare function argument.
knownGenericIgnore :: QPi -> Int
knownGenericIgnore f = ordinaryIgnore f

-- | Leave the proof and function unused in the full worker expression.
knownIgnoreWorker :: QPi -> Int
knownIgnoreWorker f = ignoreQ (withQ (error "native-known-bottom-proof") f)

-- | Keep the actual implicit witness application without an eta replacement.
knownTransport :: QPi -> QPi
knownTransport f = withQ Refl f

-- | Keep the actual named witness application in the function result.
knownNamedTransport :: QPi -> QPi
knownNamedTransport f = withQ qRefl f

-- | Use the complete witness expectation for ordinary error instantiation.
knownDemandProof :: QPi -> QPi
knownDemandProof f = withQ (error "native-known-bottom-proof") f

-- | Ignore the unavailable witness while returning the original function.
knownIgnoreProof :: QPi -> QPi
knownIgnoreProof f = ignoreProofQ (error "native-known-bottom-proof") f

-- | Check the partial application from its complete expected arrow.
knownPartialProof :: QPi -> QPi
knownPartialProof = withQ (error "native-known-bottom-proof")

-- | Demand the tag through the dictionary between the erased quantifiers.
knownDemandTag :: QPi -> QPi
knownDemandTag f = withEqQ @Int Refl (error "native-known-bottom-tag") f
