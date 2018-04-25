{-# LANGUAGE ExplicitForAll #-}

module ShouldCompile where

identity :: forall a. a -> a
identity x = x
