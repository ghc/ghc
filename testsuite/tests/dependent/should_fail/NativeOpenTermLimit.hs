{-# LANGUAGE ExperimentalDependentTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -freduction-depth=1 #-}
module NativeOpenTermLimit where

x :: forall a. Num a => id a
x = 42
