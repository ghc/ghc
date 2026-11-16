{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Export workers with actual retained function binders.
module FunctionSignatureLibrary (idCall, numericCall, specializedCall, allocatedCall,
    transportCall, shadowedCall, forceCall, forcePartialCall,
    selectCall, forceExplicitCall, forceNestedCall) where

import PackedVecLibrary

-- | Return the original vector without evaluating its function or input index.
idCall :: forall a. foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec a (f n) -> Vec a (f n)
idCall f n xs = xs

{- Note [Source fixed function-domain comparison]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The ordinary payload parameter precedes the retained function and scalar
binders. The expected complete worker type can select that parameter without
applying either retained argument. The explicit wildcard in allocatedCall
allocates the parameter first. Its body equality must then solve that meta
below both binders. See Note [Solving retained function equalities] in
GHC.Tc.Solver.Equality.

forceValue has an ordinary lifted signature. Native known-value matching can
select its parameter from a complete worker type. A complete explicit type
argument can also select that type. Direct seq accepts the complete lifted
worker argument in this NoImpredicativeTypes module. Outer Quick Look checking
has separate restrictions.
The source controls check that neither comparison nor forcing adds an eta
expansion that changes function WHNF.
-}

-- | Construct a vector after the runtime function determines its length.
numericCall :: forall a. Num a => foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec a (f n)
numericCall f n = case f n of
    Z -> let xs :: Vec a (f n); xs = VNil in xs
    S k -> let xs :: Vec a (f n); xs = VCons 42 (replicateNative k 42) in xs

-- | Select Int without supplying the retained function argument.
specializedCall :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n)
specializedCall = numericCall

-- | Solve a preallocated payload parameter below both retained binders.
allocatedCall :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n)
allocatedCall = numericCall @_

-- | Use an ordinary Given while comparing the complete retained bodies.
transportCall :: forall a b. a ~ b =>
    (foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec a (f n)) ->
    (foreach (g :: Nat -> Nat) -> foreach (m :: Nat) -> Vec b (g m))
transportCall worker = worker

-- | Keep the inner and outer same-spelling function binders distinct.
shadowedCall :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n)
shadowedCall f n =
    let worker :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n)
        worker = numericCall
    in worker f n

-- | Force an ordinary lifted value to WHNF.
forceValue :: forall a. a -> IO ()
forceValue value = value `seq` pure ()

-- | Force the complete worker without applying either retained argument.
-- See Note [Source fixed function-domain comparison].
forceCall :: (foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n)) -> IO ()
forceCall worker = forceValue worker

-- | Force a partial worker without forcing its captured function argument.
forcePartialCall :: foreach (f :: Nat -> Nat) ->
    (foreach (n :: Nat) -> Vec Int (f n)) -> IO ()
forcePartialCall f worker = seq @(foreach (n :: Nat) -> Vec Int (f n)) worker (pure ())

-- | Select a complete worker with a written type argument.
selectCall ::
    (foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n)) ->
    (foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n))
selectCall worker =
    id @(foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n)) worker

-- | Force a worker with a complete explicit type argument to seq.
forceExplicitCall :: (foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n)) -> IO ()
forceExplicitCall worker =
    seq @(foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n)) worker (pure ())

-- | Force the outer constructor without forcing its retained worker field.
forceNestedCall :: (foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n)) -> IO ()
forceNestedCall worker =
    forceValue @(Maybe (foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n))) (Just worker)
