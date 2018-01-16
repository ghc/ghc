{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module T9211 where

-- foo :: (forall f g. (Functor f) => f a -> f b) -> [a] -> [b]
foo :: (forall f g. (Functor f, g ~ f) => g a -> g b) -> [a] -> [b]
foo tr x = tr x

t = foo (fmap not) [True]
