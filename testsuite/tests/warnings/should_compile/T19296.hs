{-# OPTIONS_GHC -Wredundant-constraints #-}

module M ( f ) where

f :: Eq a => a -> ()
f _ = ()

g _ = (\x -> ()) :: Eq a => a -> ()

{-# SPECIALISE foo :: Eq b => (Int, b) -> (Int, b) #-}

foo :: Ord a => (a, b) -> (a, b)
foo = id
