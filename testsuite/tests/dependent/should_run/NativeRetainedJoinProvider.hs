{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module NativeRetainedJoinProvider (Stored(..), store) where

data Stored where
    Stored :: foreach (b :: Bool) ->
              (case b of { True -> Int; False -> Char }) -> Stored

store :: Bool -> Stored
store True = Stored True 37
store False = Stored False 'd'
