{-# LANGUAGE ExperimentalDependentTypes #-}
module CharacterCase (upper, flag, upperAlias, flagAlias) where

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

-- | Forward to the checked character mapping.
upperAlias :: Char -> Char
upperAlias = upper

-- | Forward to the checked character predicate.
flagAlias :: Char -> Bool
flagAlias = flag
