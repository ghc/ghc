{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module T11608 where

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where
  each :: Traversal s t a b
  default each :: (Traversable g, s ~ g a, t ~ g b) => Traversal s t a b
  each = traverse
