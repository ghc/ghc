{-# LANGUAGE RankNTypes #-}

module T4426 where

-- GHC 8.0 no longer allows implicit quantification
-- on the RHS.  This is more consistent:
--       type F a = a -> m a
-- has always been rejected and hence so should
-- these four.  (It was already deprecated in 7.10.)
type F a = Monad m => a -> m a

data X a = X (Eq b => a -> b)

data Y a = Y { k :: Eq b => a -> b -> c }

f :: forall b. (Monad m => m b) -> b
f = undefined

-- But these ones are fine:
type F' a = forall m. Monad m => a -> m a

data X' a = X' (forall b. Eq b => a -> b)

data Y' a = Y' { k' :: forall b c. Eq b => a -> b -> c }

f' :: forall b. (forall m. Monad m => m b) -> b
f' = undefined
