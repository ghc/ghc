{-# LANGUAGE ExperimentalDependentTypes, DataKinds #-}
module NativeCSEProjectionLib (lazyWitness) where

import GHC.Sigma (Sigma(Pair))

type Constant = some (tag :: Bool) | Int

lazyWitness :: Constant
lazyWitness = let bottom = error "witness forced" :: Bool in Pair bottom 7
