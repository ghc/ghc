{-# LANGUAGE TypeFamilies, RankNTypes #-}

module T4492 where

type family F a b
type instance F (Maybe a) b = b -> F a b

class C a where
   go :: (forall a. Maybe a -> b -> a) -> a -> F a b

instance C a => C (Maybe a) where
   go f a b = go f (f a b)
