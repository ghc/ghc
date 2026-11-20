{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds #-}
module NativeTypeCaseFallback where

type Force (b :: Bool) = case b of { True -> Int; _ -> Int }
type Always (b :: Bool) = case b of { _ -> Int }
type Echo (a :: k) = case a of { x -> x }
type Shadow (x :: Bool) (b :: Bool) = case b of { True -> x; x -> x }
type Nested (m :: Maybe Bool) = case m of { Just True -> Int; _ -> Char }
