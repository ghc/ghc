{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

module T18036 where

class Fold f where
    fold :: Monoid m => f m -> m

newtype Identity a = Identity a

-- Here we /should/ warn about redundant constraints in the
-- instance signature, since we can remove them
instance Fold Identity where
    fold :: Monoid a => Identity a -> a
    fold (Identity a) = a
