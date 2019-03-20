module PragmaDocs where

{-# DEPRECATED contains "Use `elem` instead." #-}
contains :: (Eq a, Foldable f) => f a -> a -> Bool
contains = flip elem

{-# warning x, y "These are useless" #-}
x = ()
y = ()
