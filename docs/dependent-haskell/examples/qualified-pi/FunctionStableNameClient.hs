{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExperimentalDependentTypes #-}
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
