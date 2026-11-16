{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImpredicativeTypes #-}

module FunctionStableNameClient where

import QualifiedPiLibrary
import System.Mem.StableName (makeStableName)

-- | Probe ordinary-result inference without spelling StableName QPi in source.
sameFunction :: QPi -> QPi -> IO Bool
sameFunction f g = do
    first <- makeStableName f
    second <- makeStableName g
    pure (first == second)

-- | Compare plain function values without a source StableName Pi operand.
samePlainFunction :: PlainPi -> PlainPi -> IO Bool
samePlainFunction f g = do
    first <- makeStableName f
    second <- makeStableName g
    pure (first == second)
