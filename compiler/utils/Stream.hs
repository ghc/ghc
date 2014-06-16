-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2012
--
-- Monadic streams
--
-- -----------------------------------------------------------------------------

module Stream (
    Stream(..), yield, liftIO,
    collect, fromList,
    Stream.map, Stream.mapM, Stream.mapAccumL
  ) where
import Control.Monad
import Control.Applicative

-- |
-- @Stream m a b@ is a computation in some Monad @m@ that delivers a sequence
-- of elements of type @a@ followed by a result of type @b@.
--
-- More concretely, a value of type @Stream m a b@ can be run using @runStream@
-- in the Monad @m@, and it delivers either
--
--  * the final result: @Left b@, or
--  * @Right (a,str)@, where @a@ is the next element in the stream, and @str@
--    is a computation to get the rest of the stream.
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
newtype Stream m a b = Stream { runStream :: m (Either b (a, Stream m a b)) }

instance Monad f => Functor (Stream f a) where
  fmap = liftM

instance Monad m => Applicative (Stream m a) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (Stream m a) where
  return a = Stream (return (Left a))

  Stream m >>= k = Stream $ do
                r <- m
                case r of
                  Left b        -> runStream (k b)
                  Right (a,str) -> return (Right (a, str >>= k))

yield :: Monad m => a -> Stream m a ()
yield a = Stream (return (Right (a, return ())))

liftIO :: IO a -> Stream IO b a
liftIO io = Stream $ io >>= return . Left

-- | Turn a Stream into an ordinary list, by demanding all the elements.
collect :: Monad m => Stream m a () -> m [a]
collect str = go str []
 where
  go str acc = do
    r <- runStream str
    case r of
      Left () -> return (reverse acc)
      Right (a, str') -> go str' (a:acc)

-- | Turn a list into a 'Stream', by yielding each element in turn.
fromList :: Monad m => [a] -> Stream m a ()
fromList = mapM_ yield

-- | Apply a function to each element of a 'Stream', lazily
map :: Monad m => (a -> b) -> Stream m a x -> Stream m b x
map f str = Stream $ do
   r <- runStream str
   case r of
     Left x -> return (Left x)
     Right (a, str') -> return (Right (f a, Stream.map f str'))

-- | Apply a monadic operation to each element of a 'Stream', lazily
mapM :: Monad m => (a -> m b) -> Stream m a x -> Stream m b x
mapM f str = Stream $ do
   r <- runStream str
   case r of
     Left x -> return (Left x)
     Right (a, str') -> do
        b <- f a
        return (Right (b, Stream.mapM f str'))

-- | analog of the list-based 'mapAccumL' on Streams.  This is a simple
-- way to map over a Stream while carrying some state around.
mapAccumL :: Monad m => (c -> a -> m (c,b)) -> c -> Stream m a ()
          -> Stream m b c
mapAccumL f c str = Stream $ do
  r <- runStream str
  case r of
    Left  () -> return (Left c)
    Right (a, str') -> do
      (c',b) <- f c a
      return (Right (b, mapAccumL f c' str'))
