{-# LANGUAGE RankNTypes, BangPatterns, ScopedTypeVariables,
  MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

{-# OPTIONS -fexpose-all-unfoldings #-}



module EffBench where



times :: Monad m => Int -> m a -> m ()
times n ma = go n where
  go 0 = pure ()
  go n = ma >> go (n - 1)
{-# inline times #-}


-- inlined church free state
--------------------------------------------------------------------------------
-- IF, we run a late pass of SAT and modify the bound to run SAT on one
-- static argument AND add an additional run of the simplifier then this
-- optimises well. AND also need to run specConstr after doing SAT.

newtype CS s a = CS {runCS ::
     forall r.
     (a -> r)          -- pure
  -> ((s -> r) -> r)   -- get
  -> (s -> r -> r)     -- put
  -> r
  }

instance Functor (CS s) where

  fmap f (CS g) = CS $ \pure get put -> g (pure . f) get put
  {-# inline fmap #-}

instance Applicative (CS s) where
  pure a = CS $ \pure get put -> pure a
  {-# inline pure #-}
  CS mf <*> CS ma = CS $ \pure get put ->
    mf (\f -> ma (pure . f) get put) get put
  {-# inline (<*>) #-}

instance Monad (CS s) where
  return a = CS $ \pure get put -> pure a
  {-# inline return #-}
  CS ma >>= f = CS $ \pure get put ->
    ma (\a -> runCS (f a) pure get put) get put
  {-# inline (>>=) #-}
  CS ma >> CS mb = CS $ \pure get put -> ma (\_ -> mb pure get put) get put
  {-# inline (>>) #-}

cmodify :: (s -> s) -> CS s ()
cmodify f = CS $ \pure get put ->
  get $ \s -> let !s' = f s in
  put s' $
  pure ()
{-# inline cmodify #-}

crunState :: CS s a -> s -> (a, s)
crunState (CS f) = f
  (\a s -> (a, s))
  (\got s -> got s s)
  (\s' put s -> put s')
{-# inline crunState #-}

test2 :: Int -> ((), Int)
test2 n = crunState (times n (cmodify (+1))) 0


