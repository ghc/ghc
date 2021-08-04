-- |
-- Module      : Basement.Compat.MonadTrans
-- License     : BSD-style
-- Maintainer  : Psychohistorians
-- Stability   : experimental
-- Portability : portable
--
-- An internal and really simple monad transformers,
-- without any bells and whistse.
module Basement.Compat.MonadTrans
    ( State(..)
    , Reader(..)
    ) where

import Basement.Compat.Base
import Control.Monad ((>=>))

-- | Simple State monad
newtype State s m a = State { runState :: s -> m (a, s) }

instance Monad m => Functor (State s m) where
    fmap f fa = State $ runState fa >=> (\(a, s2) -> return (f a, s2))
instance Monad m => Applicative (State s m) where
    pure a = State $ \st -> return (a,st)
    fab <*> fa = State $ \s1 -> do
        (ab,s2) <- runState fab s1
        (a,s3)  <- runState fa s2
        return (ab a, s3)
instance Monad m => Monad (State r m) where
    return a = State $ \st -> return (a,st)
    ma >>= mb = State $ \s1 -> do
        (a,s2) <- runState ma s1
        runState (mb a) s2

-- | Simple Reader monad
newtype Reader r m a = Reader { runReader :: r -> m a }

instance Monad m => Functor (Reader r m) where
    fmap f fa = Reader $ runReader fa >=> (\a -> return (f a))
instance Monad m => Applicative (Reader r m) where
    pure a = Reader $ \_ -> return a
    fab <*> fa = Reader $ \r -> do
        a  <- runReader fa r
        ab <- runReader fab r
        return $ ab a
instance Monad m => Monad (Reader r m) where
    return a = Reader $ \_ -> return a
    ma >>= mb = Reader $ \r -> do
        a <- runReader ma r
        runReader (mb a) r
