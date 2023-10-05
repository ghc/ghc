{-# OPTIONS_GHC -fdefer-type-errors #-}

module T20083 where

ap :: (a -> b) -> a %m -> b
ap f x = f x

ap2 :: a %1-> ()
ap2 _ = ()
