{-# LANGUAGE GADTs, AllowAmbiguousTypes #-}
module T8392a where

-- Should complain even with AllowAmbiguousTypes

foo :: (Int ~ Bool) => a -> a
foo x = x
