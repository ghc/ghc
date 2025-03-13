{-# LANGUAGE PatternSynonyms #-}

module RecompCompleteFixityA where

-- Define an infix pattern synonym
pattern (:!) :: a -> [a] -> [a]
pattern x :! xs = x : xs

-- Declare fixity for the pattern synonym
infixr 5 :!
