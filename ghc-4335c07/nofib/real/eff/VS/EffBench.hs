{-# LANGUAGE RankNTypes, BangPatterns, ScopedTypeVariables,
  MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

{-# OPTIONS -fexpose-all-unfoldings #-}



module EffBench where


times :: Monad m => Int -> m a -> m ()
times n ma = go n where
  go 0 = pure ()
  go n = ma >> go (n - 1)
{-# inline times #-}



-- inlined van Laarhoven free state
--------------------------------------------------------------------------------

newtype S s a = S {runS :: s -> (a, s)}

newtype VS s a = VS { runVS ::
     forall m.
     (forall a. a -> m a)                   -- pure
  -> (forall a b. m a -> (a -> m b) -> m b) -- bind
  -> m s                                  -- get
  -> (s -> m ())                            -- put
  -> m a
  }

instance Functor (VS s) where
  fmap f (VS g) = VS $ \pure (>>=) get put ->
    g pure (>>=) get put >>= \a -> pure (f a)
  {-# inline fmap #-}

instance Applicative (VS s) where
  pure a = VS $ \pure (>>=) get put -> pure a
  VS mf <*> VS ma = VS $ \pure (>>=) get put ->
    mf pure (>>=) get put >>= \f ->
    ma pure (>>=) get put >>= \a -> pure (f a)
  {-# inline pure #-}
  {-# inline (<*>) #-}

instance Monad (VS s) where
  return a = VS $ \pure (>>=) get put -> pure a
  VS ma >>= f = VS $ \pure (>>=) get put ->
    ma pure (>>=) get put >>= \a -> runVS (f a) pure (>>=) get put
  {-# inline return #-}

vmodify :: (s -> s) -> VS s ()
vmodify f = VS $ \pure (>>=) get put ->
  get >>= \s ->
  let !s' = f s in
  put s'
{-# inline vmodify #-}

vrunState' :: VS s a -> S s a
vrunState' (VS f) = f
  (\a -> S $ \s -> (a, s))
  (\(S ma) f -> S $ \s -> let !(!a, !s') = ma s; !(!b, !s'') = runS (f a) s' in (b, s''))
  (S $ \s -> (s, s))
  (\s' -> S $ const ((), s'))
{-# inline vrunState' #-}

vrunState :: VS s a -> s -> (a, s)
vrunState x = runS (vrunState' x)
{-# inline vrunState #-}

