{-# LANGUAGE ExperimentalDependentTypes #-}
module CharacterCase (upper) where

-- | Map two characters and preserve every other input.
upper :: Char -> Char
upper 'x' = 'X'
upper 'é' = 'É'
upper c   = c
