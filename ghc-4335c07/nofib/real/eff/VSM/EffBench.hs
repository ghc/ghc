{-# LANGUAGE RankNTypes, BangPatterns, ScopedTypeVariables,
  MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

{-# OPTIONS -fexpose-all-unfoldings #-}



module EffBench where

import qualified Control.Monad.State.Strict as S


times :: Monad m => Int -> m a -> m ()
times n ma = go n where
  go 0 = pure ()
  go n = ma >> go (n - 1)
{-# inline times #-}


-- van Laarhoven state translated to mtl State
--------------------------------------------------------------------------------

newtype VSM s a = VSM { runVSM :: forall m. Monad m => m s -> (s -> m ()) -> m a}

instance Functor (VSM s) where
  fmap f (VSM g) = VSM $ \get put ->
    g get put >>= \a -> pure (f a)
  {-# inline fmap #-}

instance Applicative (VSM s) where
  pure a = VSM $ \get put -> pure a
  VSM mf <*> VSM ma = VSM $ \get put ->
    mf get put >>= \f ->
    ma get put >>= \a -> pure (f a)
  {-# inline pure #-}
  {-# inline (<*>) #-}

instance Monad (VSM s) where
  return a = VSM $ \get put -> pure a
  VSM ma >>= f = VSM $ \get put ->
    ma get put >>= \a -> runVSM (f a) get put
  {-# inline return #-}

vmmodify :: (s -> s) -> VSM s ()
vmmodify f = VSM $ \get put ->
  get >>= \s ->
  let !s' = f s in
  put s'
{-# inline vmmodify #-}

vmrunState :: VSM s a -> s -> (a, s)
vmrunState (VSM ma) = S.runState (ma S.get S.put)
{-# inline vmrunState #-}

test :: Int -> ((), Int)
test n = vmrunState (times n $ vmmodify (+1)) n


