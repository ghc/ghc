{-# OPTIONS_GHC -Wredundant-constraints #-}

module M ( f ) where

f :: Eq a => a -> ()
f _ = ()

g _ = (\x -> ()) :: Eq a => a -> ()
