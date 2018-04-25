{-# LANGUAGE RankNTypes, LambdaCase, ScopedTypeVariables,
  MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

{-# OPTIONS -fexpose-all-unfoldings #-}



module EffBench where


times :: Monad m => Int -> m a -> m ()
times n ma = go n where
  go 0 = pure ()
  go n = ma >> go (n - 1)
{-# inline times #-}

-- inlined free state
--------------------------------------------------------------------------------

data FS s a = Pure a | Get (s -> FS s a) | Put !s (FS s a)

instance Functor (FS s) where
  fmap f = go where
    go = \case
      Pure a  -> Pure (f a)
      Get k   -> Get (fmap f . k)
      Put s k -> Put s (fmap f k)
  {-#  inline fmap #-}

instance Applicative (FS s) where
  pure = Pure
  Pure f  <*> ma = fmap f ma
  Get k   <*> ma = Get ((<*> ma) . k)
  Put s k <*> ma = Put s (k <*> ma)
  {-# inline pure #-}
  {-# inline (<*>) #-}

instance Monad (FS s) where
  return = Pure
  Pure a  >>= f = f a
  Get k   >>= f = Get ((>>= f) . k)
  Put s k >>= f = Put s (k >>= f)
  {-# inline return #-}
  {-# inline (>>=) #-}

fmodify :: (s -> s) -> FS s ()
fmodify f =
  Get $ \s ->
  Put (f s) $
  Pure ()
{-# inline fmodify #-}

frunState :: FS s a -> s -> (a, s)
frunState (Pure a)   s = (a, s)
frunState (Get k)    s = frunState (k s) s
frunState (Put s' k) s = frunState k s'

