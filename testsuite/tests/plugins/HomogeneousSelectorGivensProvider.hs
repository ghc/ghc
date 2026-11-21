{-# LANGUAGE GHC2024, NoExperimentalDependentTypes #-}
module HomogeneousSelectorGivensProvider where

type role Premise nominal
data Premise (a :: k) = Premise

type role Result nominal
data Result (a :: k) = Result Int deriving (Show)

type role RepPremise representational
data RepPremise (a :: k) = RepPremise

type role RepResult representational
data RepResult (a :: k) = RepResult Int deriving (Show)

newtype Age = Age Int
