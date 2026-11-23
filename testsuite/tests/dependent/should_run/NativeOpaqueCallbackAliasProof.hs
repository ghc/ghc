{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}

module NativeOpaqueCallbackAliasProof (Witness(..), token, accepts, runCallback) where

data Witness (out :: Char) = Witness

token :: foreach (f :: Bool -> Char) -> foreach (x :: Bool) -> Witness (f x)
token _ _ = Witness

accepts :: foreach (f :: Bool -> Char) -> foreach (x :: Bool) -> Witness (f x) -> Bool
accepts _ _ Witness = True

runCallback :: foreach (f :: Bool -> Char) -> foreach (x :: Bool) -> (Char, Witness (f x))
runCallback f x = (f x, Witness)
