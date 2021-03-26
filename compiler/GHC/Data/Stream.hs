{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2012
--
-- -----------------------------------------------------------------------------

-- | Monadic streams
module GHC.Data.Stream (
    Stream(..), StreamS(..), runStream, yield, liftIO,
    collect,  consume, fromList,
    map, mapM, mapAccumL_
  ) where

import GHC.Prelude hiding (map,mapM)

import Control.Monad hiding (mapM)
import Control.Monad.IO.Class

-- |
-- @Stream m a b@ is a computation in some Monad @m@ that delivers a sequence
-- of elements of type @a@ followed by a result of type @b@.
--
-- More concretely, a value of type @Stream m a b@ can be run using @runStreamInternal@
-- in the Monad @m@, and it delivers either
--
--  * the final result: @Done b@, or
--  * @Yield a str@ where @a@ is the next element in the stream, and @str@
--     is the rest of the stream
--  * @Effect mstr@ where @mstr@ is some action running in @m@ which
--  generates the rest of the stream.
--
-- Stream is itself a Monad, and provides an operation 'yield' that
-- produces a new element of the stream.  This makes it convenient to turn
-- existing monadic computations into streams.
--
-- The idea is that Stream is useful for making a monadic computation
-- that produces values from time to time.  This can be used for
-- knitting together two complex monadic operations, so that the
-- producer does not have to produce all its values before the
-- consumer starts consuming them.  We make the producer into a
-- Stream, and the consumer pulls on the stream each time it wants a
-- new value.
--
-- 'Stream' is implemented in the "yoneda" style for efficiency. By
-- representing a stream in this manner 'fmap' and '>>=' operations are
-- accumulated in the function parameters before being applied once when
-- the stream is destroyed. In the old implementation each usage of 'mapM'
-- and '>>=' would traverse the entire stream in order to apply the
-- substitution at the leaves.
--
-- The >>= operation for 'Stream' was a hot-spot in the ticky profile for
-- the "ManyConstructors" test which called the 'cg' function many times in
-- @StgToCmm.hs@
--
newtype Stream m a b =
          Stream { runStreamInternal :: forall r' r .
                                        (a -> m r') -- For fusing calls to `map` and `mapM`
                                     -> (b -> StreamS m r' r)  -- For fusing `>>=`
                                     -> StreamS m r' r }

runStream :: Applicative m => Stream m r' r -> StreamS m r' r
runStream st = runStreamInternal st pure Done

data StreamS m a b = Yield a (StreamS m a b)
                   | Done b
                   | Effect (m (StreamS m a b))

instance Monad m => Functor (StreamS m a) where
  fmap = liftM

instance Monad m => Applicative (StreamS m a) where
  pure = Done
  (<*>) = ap

instance Monad m => Monad (StreamS m a) where
  a >>= k = case a of
                      Done r -> k r
                      Yield a s -> Yield a (s >>= k)
                      Effect m -> Effect (fmap (>>= k) m)

instance Functor (Stream f a) where
  fmap = liftM

instance Applicative (Stream m a) where
  pure a = Stream $ \_f g -> g a
  (<*>) = ap

instance Monad (Stream m a) where
  Stream m >>= k = Stream $ \f h -> m f (\a -> runStreamInternal (k a) f h)

instance MonadIO m => MonadIO (Stream m b) where
  liftIO io = Stream $ \_f g -> Effect (g <$> liftIO io)

yield :: Monad m => a -> Stream m a ()
yield a = Stream $ \f rest -> Effect (flip Yield (rest ())  <$> f a)

-- | Turn a Stream into an ordinary list, by demanding all the elements.
collect :: Monad m => Stream m a () -> m [a]
collect str = go [] (runStream str)
 where
  go acc (Done ()) = return (reverse acc)
  go acc (Effect m) = m >>= go acc
  go acc (Yield a k) = go (a:acc) k

consume :: (Monad m, Monad n) => Stream m a b -> (forall a . m a -> n a) -> (a -> n ()) -> n b
consume str l f = go (runStream str)
  where
    go (Done r) = return r
    go (Yield a p) = f a >> go p
    go (Effect m)  = l m >>= go

-- | Turn a list into a 'Stream', by yielding each element in turn.
fromList :: Monad m => [a] -> Stream m a ()
fromList = mapM_ yield

-- | Apply a function to each element of a 'Stream', lazily
map :: Monad m => (a -> b) -> Stream m a x -> Stream m b x
map f str = Stream $ \g h -> runStreamInternal str (g . f) h

-- | Apply a monadic operation to each element of a 'Stream', lazily
mapM :: Monad m => (a -> m b) -> Stream m a x -> Stream m b x
mapM f str = Stream $ \g h -> runStreamInternal str (g <=< f) h

-- | Note this is not very efficient because it traverses the whole stream
-- before rebuilding it, avoid using it if you can. mapAccumL used to
-- implemented but it wasn't used anywhere in the compiler and has similar
-- effiency problems.
mapAccumL_ :: forall m a b c r . Monad m => (c -> a -> m (c,b)) -> c -> Stream m a r
           -> Stream m b (c, r)
mapAccumL_ f c str = Stream $ \f h -> go c f h (runStream str)

  where
    go :: c
             -> (b -> m r')
             -> ((c, r) -> StreamS m r' r1)
             -> StreamS m a r
             -> StreamS m r' r1
    go c _f1 h1 (Done r) = h1 (c, r)
    go c f1 h1 (Yield a p) = Effect (f c a >>= (\(c', b) -> f1 b
                                           >>= \r' -> return $ Yield r' (go c' f1 h1 p)))
    go c f1 h1 (Effect m) = Effect (go c f1 h1 <$> m)
