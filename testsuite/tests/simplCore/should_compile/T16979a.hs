{-# LANGUAGE RankNTypes #-}
module T16979a where

data Id a
instance Functor Id where fmap = undefined

newtype Y f a = MkY (forall b. Bool -> f b)
instance Functor (Y f) where fmap = undefined

hm :: Id Int
hm = x

weird :: Y f a ->  f b
weird (MkY g) = g True
{-# INLINE weird #-}

x :: Functor g => g Int
x = weird (weird (weird (weird x)))
