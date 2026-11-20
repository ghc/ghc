{-# LANGUAGE NoExperimentalDependentTypes #-}
module OpenMapWorker (each, Lamp(..), Signal(..), signal, mayProceed) where

-- | Apply a function to each element and keep the result fields lazy.
each :: (a -> b) -> [a] -> [b]
each _ [] = []
each f (x:xs) = f x : each f xs

-- | Two colours of a traffic light.
data Lamp = Red | Green

-- | Instructions for approaching traffic.
data Signal = Stop | Go

-- | Translate a light colour into an instruction.
signal :: Lamp -> Signal
signal Red = Stop
signal Green = Go

-- | A red light stops traffic without consulting the permission.
mayProceed :: Lamp -> Bool -> Bool
mayProceed Red _ = False
mayProceed Green allowed = allowed
