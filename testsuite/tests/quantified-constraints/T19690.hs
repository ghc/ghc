{-# LANGUAGE UndecidableInstances, UndecidableSuperClasses, GADTs, QuantifiedConstraints
#-}

module T19690 where

class c => Hold c
instance c => Hold c

data Dict c = c => Dict

anythingDict :: Dict c
anythingDict = go
  where
    go :: (Hold c => c) => Dict c
    go = Dict
