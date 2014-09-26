{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fwarn-context-quantification #-}

module T4426 where

type F a = Monad m => a -> m a

data X a = X (Eq b => a -> b)

data Y a = Y { k :: Eq b => a -> b -> c }

f :: forall b. (Monad m => m b) -> b
f = undefined

type F' a = forall m. Monad m => a -> m a

data X' a = X' (forall b. Eq b => a -> b)

data Y' a = Y' { k' :: forall b c. Eq b => a -> b -> c }

f' :: forall b. (forall m. Monad m => m b) -> b
f' = undefined
