{-# LANGUAGE KindSignatures #-}

module T12425 where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Lazy (StateT(..))

data Result a m b = RecurseOnly (Maybe (CondT a m b))
                  | KeepAndRecurse b (Maybe (CondT a m b))

instance Monad m => Functor (Result a m) where
    fmap f (RecurseOnly l)      = RecurseOnly (liftM (fmap f) l)
    fmap f (KeepAndRecurse a l) = KeepAndRecurse (f a) (liftM (fmap f) l)
    {-# INLINE fmap #-}

newtype CondT a m b = CondT (StateT a m (Result a m b))

instance Monad m => Functor (CondT a m) where
    fmap f (CondT g) = CondT (liftM (fmap f) g)
    {-# INLINE fmap #-}

instance Monad m => Applicative (CondT a m) where
    pure  = undefined
    (<*>) = undefined

instance Monad m => Monad (CondT a m) where
    return = undefined
    (>>=) = undefined

-- liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
