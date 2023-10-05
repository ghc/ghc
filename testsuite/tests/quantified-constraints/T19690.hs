{-# LANGUAGE UndecidableInstances, UndecidableSuperClasses, ConstraintKinds, FlexibleInstances,
  GADTs, QuantifiedConstraints, TypeApplications, ScopedTypeVariables #-}

module T19690 where

class c => Hold c
instance c => Hold c

data Dict c = c => Dict

anythingDict :: Dict c
anythingDict = go
  where
    go :: (Hold c => c) => Dict c
    go = Dict
