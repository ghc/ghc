{-# LANGUAGE ExperimentalDependentTypes #-}

module ClosedTableLibrary (flipBit, Lamp(..), Signal(..), signal) where

-- | Invert a Boolean by matching its two constructors.
flipBit :: Bool -> Bool
flipBit False = True
flipBit True  = False

-- | The three colours of a traffic light.
data Lamp = Red | Amber | Green

-- | The instruction given by the light.
data Signal = Stop | Go deriving Show

-- | Infer the domain and result of this table from its constructors.
signal Red   = Stop
signal Amber = Stop
signal Green = Go
