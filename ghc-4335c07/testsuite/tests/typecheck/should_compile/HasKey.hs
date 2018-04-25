{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- Provided by Christian Maeder; broke
-- a pre-release GHC 7.0

module HasKey where

class Ord key => HasKey x key | x -> key where
   toKey :: x -> key

newtype Keyed x = Keyed { unKey :: x }

lift :: (HasKey x1 key1,HasKey x2 key2)
   => (key1 -> key2 -> a) -> (Keyed x1 -> Keyed x2 -> a)
lift f x1 x2 = f (toKey . unKey $ x1) (toKey . unKey $ x2)

instance HasKey x key => Eq (Keyed x) where
   (==) = lift (==)

instance HasKey x key => Ord (Keyed x)
