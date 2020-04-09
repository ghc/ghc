{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

module T18036 where

class Fold f where
    fold :: Monoid m => f m -> m

newtype Identity a = Identity a

instance Fold Identity where
    fold :: Identity a -> a
    fold (Identity a) = a
