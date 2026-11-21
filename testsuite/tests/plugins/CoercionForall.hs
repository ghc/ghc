{-# LANGUAGE GHC2024, NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fplugin=CoercionForallPlugin #-}
module CoercionForall where

type role Index nominal
data Index (a :: k) = Index Int
