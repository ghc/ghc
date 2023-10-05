{-# LANGUAGE BangPatterns, Rank2Types, FlexibleContexts, LambdaCase, DeepSubsumption #-}
module DeepSubsumption02 where

import Data.Semigroup

-- | Finite source
type Source r s = Tap r (Maybe s)

newtype Sink t m a = Sink
  { unSink :: forall r. t m -> (a -> t m -> m r) -> m r }

-- | Mono in/out
type Converter p q r s m = Source r s (Sink (Source p q) m)

type Pipe a b m = forall r. (Monoid r, Semigroup r) => Converter r a r b m

newtype Tap r s m = Tap { unTap :: r -> m (s, Tap r s m) }

type Distiller tap r s m = Tap r s (Sink tap m)

filter :: Monad m => (a -> Bool) -> Pipe a a m
--filter f = filtering $ maybe True f
filter = filtering . maybe True
{-# INLINE filter #-}

mapAccum :: Monad m => (s -> a -> (s, b)) -> s -> Pipe a b m
--mapAccum f x = go x where
mapAccum f = go where
  go s = reservingTap $ \case
    Just a -> let (s', b) = f s a in return (Just b, go s')
    Nothing -> return (Nothing, go s)
{-# INLINE mapAccum #-}

traverse :: (Monad m) => (a -> m b) -> Pipe a b m
-- traverse f = traversing $ Prelude.traverse f
traverse = traversing . Prelude.traverse
{-# INLINE traverse #-}

-- | Get one element preserving a request
reservingTap :: Monad m => (a -> Sink (Tap r a) m (b, Distiller (Tap r a) r b m)) -> Distiller (Tap r a) r b m
reservingTap k = Tap $ \r -> Sink $ \t cont -> do
  (a, t') <- unTap t r
  unSink (k a) t' cont
{-# INLINE reservingTap #-}

traversing :: (Monad m) => (a -> m b) -> Distiller (Tap r a) r b m
traversing f = go where
  go = reservingTap $ \a -> do
    b <- undefined $ f a
    return (b, go)
{-# INLINE traversing #-}

filtering :: (Monoid r, Monad m) => (a -> Bool) -> Distiller (Tap r a) r a m
filtering f = go where
  go = reservingTap $ \a -> if f a
    then return (a, go)
    else unTap go mempty
{-# INLINE filtering #-}

instance Functor (Sink s m) where
  fmap f m = Sink $ \s k -> unSink m s (k . f)

instance Applicative (Sink s m) where
  pure a = Sink $ \s k -> k a s
  Sink mf <*> Sink mx = Sink
    $ \s k -> mf s $ \f s' -> mx s' $ k . f
  m *> k = m >>= \_ -> k

instance Monad (Sink s m) where
  return = pure
  {-# INLINE return #-}
  m >>= k = Sink $ \s cont -> unSink m s $ \a s' -> unSink (k a) s' cont

