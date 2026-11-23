{-# LANGUAGE ExperimentalDependentTypes #-}
module CharacterCase (upper, flag) where

-- | Map two characters and preserve every other input.
upper :: Char -> Char
upper 'x' = 'X'
upper 'é' = 'É'
upper c   = c

-- | Recognize two characters with a checked case.
flag :: Char -> Bool
flag 'x' = True
flag 'é' = True
flag _   = False
