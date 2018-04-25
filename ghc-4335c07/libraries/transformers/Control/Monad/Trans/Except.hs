{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Except
-- Copyright   :  (C) 2013 Ross Paterson
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- This monad transformer extends a monad with the ability throw exceptions.
--
-- A sequence of actions terminates normally, producing a value,
-- only if none of the actions in the sequence throws an exception.
-- If one throws an exception, the rest of the sequence is skipped and
-- the composite action exits with that exception.
--
-- If the value of the exception is not required, the variant in
-- "Control.Monad.Trans.Maybe" may be used instead.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Except (
    -- * The Except monad
    Except,
    except,
    runExcept,
    mapExcept,
    withExcept,
    -- * The ExceptT monad transformer
    ExceptT(ExceptT),
    runExceptT,
    mapExceptT,
    withExceptT,
    -- * Exception operations
    throwE,
    catchE,
    -- * Lifting other operations
    liftCallCC,
    liftListen,
    liftPass,
  ) where

import Control.Monad.IO.Class
import Control.Monad.Signatures
import Control.Monad.Trans.Class
import Data.Functor.Classes
import Data.Functor.Identity

import Control.Applicative
import Control.Monad
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif
import Control.Monad.Fix
#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip (MonadZip(mzipWith))
#endif
import Data.Foldable (Foldable(foldMap))
import Data.Monoid
import Data.Traversable (Traversable(traverse))

-- | The parameterizable exception monad.
--
-- Computations are either exceptions or normal values.
--
-- The 'return' function returns a normal value, while @>>=@ exits on
-- the first exception.  For a variant that continues after an error
-- and collects all the errors, see 'Control.Applicative.Lift.Errors'.
type Except e = ExceptT e Identity

-- | Constructor for computations in the exception monad.
-- (The inverse of 'runExcept').
except :: Either e a -> Except e a
except m = ExceptT (Identity m)
{-# INLINE except #-}

-- | Extractor for computations in the exception monad.
-- (The inverse of 'except').
runExcept :: Except e a -> Either e a
runExcept (ExceptT m) = runIdentity m
{-# INLINE runExcept #-}

-- | Map the unwrapped computation using the given function.
--
-- * @'runExcept' ('mapExcept' f m) = f ('runExcept' m)@
mapExcept :: (Either e a -> Either e' b)
        -> Except e a
        -> Except e' b
mapExcept f = mapExceptT (Identity . f . runIdentity)
{-# INLINE mapExcept #-}

-- | Transform any exceptions thrown by the computation using the given
-- function (a specialization of 'withExceptT').
withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept = withExceptT
{-# INLINE withExcept #-}

-- | A monad transformer that adds exceptions to other monads.
--
-- @ExceptT@ constructs a monad parameterized over two things:
--
-- * e - The exception type.
--
-- * m - The inner monad.
--
-- The 'return' function yields a computation that produces the given
-- value, while @>>=@ sequences two subcomputations, exiting on the
-- first exception.
newtype ExceptT e m a = ExceptT (m (Either e a))

instance (Eq e, Eq1 m) => Eq1 (ExceptT e m) where
    liftEq eq (ExceptT x) (ExceptT y) = liftEq (liftEq eq) x y
    {-# INLINE liftEq #-}

instance (Ord e, Ord1 m) => Ord1 (ExceptT e m) where
    liftCompare comp (ExceptT x) (ExceptT y) =
        liftCompare (liftCompare comp) x y
    {-# INLINE liftCompare #-}

instance (Read e, Read1 m) => Read1 (ExceptT e m) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "ExceptT" ExceptT
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl

instance (Show e, Show1 m) => Show1 (ExceptT e m) where
    liftShowsPrec sp sl d (ExceptT m) =
        showsUnaryWith (liftShowsPrec sp' sl') "ExceptT" d m
      where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList sp sl

instance (Eq e, Eq1 m, Eq a) => Eq (ExceptT e m a)
    where (==) = eq1
instance (Ord e, Ord1 m, Ord a) => Ord (ExceptT e m a)
    where compare = compare1
instance (Read e, Read1 m, Read a) => Read (ExceptT e m a) where
    readsPrec = readsPrec1
instance (Show e, Show1 m, Show a) => Show (ExceptT e m a) where
    showsPrec = showsPrec1

-- | The inverse of 'ExceptT'.
runExceptT :: ExceptT e m a -> m (Either e a)
runExceptT (ExceptT m) = m
{-# INLINE runExceptT #-}

-- | Map the unwrapped computation using the given function.
--
-- * @'runExceptT' ('mapExceptT' f m) = f ('runExceptT' m)@
mapExceptT :: (m (Either e a) -> n (Either e' b))
        -> ExceptT e m a
        -> ExceptT e' n b
mapExceptT f m = ExceptT $ f (runExceptT m)
{-# INLINE mapExceptT #-}

-- | Transform any exceptions thrown by the computation using the
-- given function.
withExceptT :: (Functor m) => (e -> e') -> ExceptT e m a -> ExceptT e' m a
withExceptT f = mapExceptT $ fmap $ either (Left . f) Right
{-# INLINE withExceptT #-}

instance (Functor m) => Functor (ExceptT e m) where
    fmap f = ExceptT . fmap (fmap f) . runExceptT
    {-# INLINE fmap #-}

instance (Foldable f) => Foldable (ExceptT e f) where
    foldMap f (ExceptT a) = foldMap (either (const mempty) f) a
    {-# INLINE foldMap #-}

instance (Traversable f) => Traversable (ExceptT e f) where
    traverse f (ExceptT a) =
        ExceptT <$> traverse (either (pure . Left) (fmap Right . f)) a
    {-# INLINE traverse #-}

instance (Functor m, Monad m) => Applicative (ExceptT e m) where
    pure a = ExceptT $ return (Right a)
    {-# INLINE pure #-}
    ExceptT f <*> ExceptT v = ExceptT $ do
        mf <- f
        case mf of
            Left e -> return (Left e)
            Right k -> do
                mv <- v
                case mv of
                    Left e -> return (Left e)
                    Right x -> return (Right (k x))
    {-# INLINEABLE (<*>) #-}

instance (Functor m, Monad m, Monoid e) => Alternative (ExceptT e m) where
    empty = ExceptT $ return (Left mempty)
    {-# INLINE empty #-}
    ExceptT mx <|> ExceptT my = ExceptT $ do
        ex <- mx
        case ex of
            Left e -> liftM (either (Left . mappend e) Right) my
            Right x -> return (Right x)
    {-# INLINEABLE (<|>) #-}

instance (Monad m) => Monad (ExceptT e m) where
#if !(MIN_VERSION_base(4,8,0))
    return a = ExceptT $ return (Right a)
    {-# INLINE return #-}
#endif
    m >>= k = ExceptT $ do
        a <- runExceptT m
        case a of
            Left e -> return (Left e)
            Right x -> runExceptT (k x)
    {-# INLINE (>>=) #-}
    fail = ExceptT . fail
    {-# INLINE fail #-}

#if MIN_VERSION_base(4,9,0)
instance (Fail.MonadFail m) => Fail.MonadFail (ExceptT e m) where
    fail = ExceptT . Fail.fail
    {-# INLINE fail #-}
#endif

instance (Monad m, Monoid e) => MonadPlus (ExceptT e m) where
    mzero = ExceptT $ return (Left mempty)
    {-# INLINE mzero #-}
    ExceptT mx `mplus` ExceptT my = ExceptT $ do
        ex <- mx
        case ex of
            Left e -> liftM (either (Left . mappend e) Right) my
            Right x -> return (Right x)
    {-# INLINEABLE mplus #-}

instance (MonadFix m) => MonadFix (ExceptT e m) where
    mfix f = ExceptT (mfix (runExceptT . f . either (const bomb) id))
      where bomb = error "mfix (ExceptT): inner computation returned Left value"
    {-# INLINE mfix #-}

instance MonadTrans (ExceptT e) where
    lift = ExceptT . liftM Right
    {-# INLINE lift #-}

instance (MonadIO m) => MonadIO (ExceptT e m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

#if MIN_VERSION_base(4,4,0)
instance (MonadZip m) => MonadZip (ExceptT e m) where
    mzipWith f (ExceptT a) (ExceptT b) = ExceptT $ mzipWith (liftA2 f) a b
    {-# INLINE mzipWith #-}
#endif

-- | Signal an exception value @e@.
--
-- * @'runExceptT' ('throwE' e) = 'return' ('Left' e)@
--
-- * @'throwE' e >>= m = 'throwE' e@
throwE :: (Monad m) => e -> ExceptT e m a
throwE = ExceptT . return . Left
{-# INLINE throwE #-}

-- | Handle an exception.
--
-- * @'catchE' h ('lift' m) = 'lift' m@
--
-- * @'catchE' h ('throwE' e) = h e@
catchE :: (Monad m) =>
    ExceptT e m a               -- ^ the inner computation
    -> (e -> ExceptT e' m a)    -- ^ a handler for exceptions in the inner
                                -- computation
    -> ExceptT e' m a
m `catchE` h = ExceptT $ do
    a <- runExceptT m
    case a of
        Left  l -> runExceptT (h l)
        Right r -> return (Right r)
{-# INLINE catchE #-}

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: CallCC m (Either e a) (Either e b) -> CallCC (ExceptT e m) a b
liftCallCC callCC f = ExceptT $
    callCC $ \ c ->
    runExceptT (f (\ a -> ExceptT $ c (Right a)))
{-# INLINE liftCallCC #-}

-- | Lift a @listen@ operation to the new monad.
liftListen :: (Monad m) => Listen w m (Either e a) -> Listen w (ExceptT e m) a
liftListen listen = mapExceptT $ \ m -> do
    (a, w) <- listen m
    return $! fmap (\ r -> (r, w)) a
{-# INLINE liftListen #-}

-- | Lift a @pass@ operation to the new monad.
liftPass :: (Monad m) => Pass w m (Either e a) -> Pass w (ExceptT e m) a
liftPass pass = mapExceptT $ \ m -> pass $ do
    a <- m
    return $! case a of
        Left l -> (Left l, id)
        Right (r, f) -> (Right r, f)
{-# INLINE liftPass #-}
