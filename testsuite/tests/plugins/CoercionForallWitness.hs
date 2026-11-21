{-# LANGUAGE GHC2024, NoExperimentalDependentTypes #-}
module CoercionForallWitness where

type role Index nominal
data Index (a :: k) = Index Int
