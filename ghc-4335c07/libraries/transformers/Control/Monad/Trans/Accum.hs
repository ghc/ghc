{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Accum
-- Copyright   :  (c) Nickolay Kudasov 2016
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The lazy 'AccumT' monad transformer, which adds accumulation
-- capabilities (such as declarations or document patches) to a given monad.
--
-- This monad transformer provides append-only accumulation
-- during the computation. For more general access, use
-- "Control.Monad.Trans.State" instead.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Accum (
    -- * The Accum monad
    Accum,
    accum,
    runAccum,
    execAccum,
    evalAccum,
    mapAccum,
    -- * The AccumT monad transformer
    AccumT(AccumT),
    runAccumT,
    execAccumT,
    evalAccumT,
    mapAccumT,
    -- * Accum operations
    look,
    looks,
    add,
    -- * Lifting other operations
    liftCallCC,
    liftCallCC',
    liftCatch,
    liftListen,
    liftPass,
    -- * Monad transformations
    readerToAccumT,
    writerToAccumT,
    accumToStateT,
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Writer (WriterT(..))
import Control.Monad.Trans.State  (StateT(..))
import Data.Functor.Identity

import Control.Applicative
import Control.Monad
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif
import Control.Monad.Fix
import Control.Monad.Signatures
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

-- ---------------------------------------------------------------------------
-- | An accumulation monad parameterized by the type @w@ of output to accumulate.
--
-- The 'return' function produces the output 'mempty', while @>>=@
-- combines the outputs of the subcomputations using 'mappend'.
type Accum w = AccumT w Identity

-- | Construct an accumulation computation from a (result, output) pair.
-- (The inverse of 'runAccum'.)
accum :: (Monad m) => (w -> (a, w)) -> AccumT w m a
accum f = AccumT $ \ w -> return (f w)
{-# INLINE accum #-}

-- | Unwrap an accumulation computation as a (result, output) pair.
-- (The inverse of 'accum'.)
runAccum :: Accum w a -> w -> (a, w)
runAccum m = runIdentity . runAccumT m
{-# INLINE runAccum #-}

-- | Extract the output from an accumulation computation.
--
-- * @'execAccum' m w = 'snd' ('runAccum' m w)@
execAccum :: Accum w a -> w -> w
execAccum m w = snd (runAccum m w)
{-# INLINE execAccum #-}

-- | Evaluate an accumulation computation with the given initial output history
-- and return the final value, discarding the final output.
--
-- * @'evalAccum' m w = 'fst' ('runAccum' m w)@
evalAccum :: (Monoid w) => Accum w a -> w -> a
evalAccum m w = fst (runAccum m w)
{-# INLINE evalAccum #-}

-- | Map both the return value and output of a computation using
-- the given function.
--
-- * @'runAccum' ('mapAccum' f m) = f . 'runAccum' m@
mapAccum :: ((a, w) -> (b, w)) -> Accum w a -> Accum w b
mapAccum f = mapAccumT (Identity . f . runIdentity)
{-# INLINE mapAccum #-}

-- ---------------------------------------------------------------------------
-- | An accumulation monad parameterized by:
--
--   * @w@ - the output to accumulate.
--
--   * @m@ - The inner monad.
--
-- The 'return' function produces the output 'mempty', while @>>=@
-- combines the outputs of the subcomputations using 'mappend'.
--
-- This monad transformer is similar to both state and writer monad transformers.
-- Thus it can be seen as
--
--  * a restricted append-only version of a state monad transformer or
--
--  * a writer monad transformer with the extra ability to read all previous output.
newtype AccumT w m a = AccumT (w -> m (a, w))

-- | Unwrap an accumulation computation.
runAccumT :: AccumT w m a -> w -> m (a, w)
runAccumT (AccumT f) = f
{-# INLINE runAccumT #-}

-- | Extract the output from an accumulation computation.
--
-- * @'execAccumT' m w = 'liftM' 'snd' ('runAccumT' m w)@
execAccumT :: (Monad m) => AccumT w m a -> w -> m w
execAccumT m w = do
    ~(_, w') <- runAccumT m w
    return w'
{-# INLINE execAccumT #-}

-- | Evaluate an accumulation computation with the given initial output history
-- and return the final value, discarding the final output.
--
-- * @'evalAccumT' m w = 'liftM' 'fst' ('runAccumT' m w)@
evalAccumT :: (Monad m, Monoid w) => AccumT w m a -> w -> m a
evalAccumT m w = do
    ~(a, _) <- runAccumT m w
    return a
{-# INLINE evalAccumT #-}

-- | Map both the return value and output of a computation using
-- the given function.
--
-- * @'runAccumT' ('mapAccumT' f m) = f . 'runAccumT' m@
mapAccumT :: (m (a, w) -> n (b, w)) -> AccumT w m a -> AccumT w n b
mapAccumT f m = AccumT (f . runAccumT m)
{-# INLINE mapAccumT #-}

instance (Functor m) => Functor (AccumT w m) where
    fmap f = mapAccumT $ fmap $ \ ~(a, w) -> (f a, w)
    {-# INLINE fmap #-}

instance (Monoid w, Functor m, Monad m) => Applicative (AccumT w m) where
    pure a  = AccumT $ const $ return (a, mempty)
    {-# INLINE pure #-}
    mf <*> mv = AccumT $ \ w -> do
      ~(f, w')  <- runAccumT mf w
      ~(v, w'') <- runAccumT mv (w `mappend` w')
      return (f v, w' `mappend` w'')
    {-# INLINE (<*>) #-}

instance (Monoid w, Functor m, MonadPlus m) => Alternative (AccumT w m) where
    empty   = AccumT $ const mzero
    {-# INLINE empty #-}
    m <|> n = AccumT $ \ w -> runAccumT m w `mplus` runAccumT n w
    {-# INLINE (<|>) #-}

instance (Monoid w, Functor m, Monad m) => Monad (AccumT w m) where
#if !(MIN_VERSION_base(4,8,0))
    return a  = AccumT $ const $ return (a, mempty)
    {-# INLINE return #-}
#endif
    m >>= k  = AccumT $ \ w -> do
        ~(a, w')  <- runAccumT m w
        ~(b, w'') <- runAccumT (k a) (w `mappend` w')
        return (b, w' `mappend` w'')
    {-# INLINE (>>=) #-}
    fail msg = AccumT $ const (fail msg)
    {-# INLINE fail #-}

#if MIN_VERSION_base(4,9,0)
instance (Monoid w, Fail.MonadFail m) => Fail.MonadFail (AccumT w m) where
    fail msg = AccumT $ const (Fail.fail msg)
    {-# INLINE fail #-}
#endif

instance (Monoid w, Functor m, MonadPlus m) => MonadPlus (AccumT w m) where
    mzero       = AccumT $ const mzero
    {-# INLINE mzero #-}
    m `mplus` n = AccumT $ \ w -> runAccumT m w `mplus` runAccumT n w
    {-# INLINE mplus #-}

instance (Monoid w, Functor m, MonadFix m) => MonadFix (AccumT w m) where
    mfix m = AccumT $ \ w -> mfix $ \ ~(a, _) -> runAccumT (m a) w
    {-# INLINE mfix #-}

instance (Monoid w) => MonadTrans (AccumT w) where
    lift m = AccumT $ const $ do
        a <- m
        return (a, mempty)
    {-# INLINE lift #-}

instance (Monoid w, Functor m, MonadIO m) => MonadIO (AccumT w m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

-- | @'look'@ is an action that fetches all the previously accumulated output.
look :: (Monoid w, Monad m) => AccumT w m w
look = AccumT $ \ w -> return (w, mempty)

-- | @'look'@ is an action that retrieves a function of the previously accumulated output.
looks :: (Monoid w, Monad m) => (w -> a) -> AccumT w m a
looks f = AccumT $ \ w -> return (f w, mempty)

-- | @'add' w@ is an action that produces the output @w@.
add :: (Monad m) => w -> AccumT w m ()
add w = accum $ const ((), w)
{-# INLINE add #-}

-- | Uniform lifting of a @callCC@ operation to the new monad.
-- This version rolls back to the original output history on entering the
-- continuation.
liftCallCC :: CallCC m (a, w) (b, w) -> CallCC (AccumT w m) a b
liftCallCC callCC f = AccumT $ \ w ->
    callCC $ \ c ->
    runAccumT (f (\ a -> AccumT $ \ _ -> c (a, w))) w
{-# INLINE liftCallCC #-}

-- | In-situ lifting of a @callCC@ operation to the new monad.
-- This version uses the current output history on entering the continuation.
-- It does not satisfy the uniformity property (see "Control.Monad.Signatures").
liftCallCC' :: CallCC m (a, w) (b, w) -> CallCC (AccumT w m) a b
liftCallCC' callCC f = AccumT $ \ s ->
    callCC $ \ c ->
    runAccumT (f (\ a -> AccumT $ \ s' -> c (a, s'))) s
{-# INLINE liftCallCC' #-}

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: Catch e m (a, w) -> Catch e (AccumT w m) a
liftCatch catchE m h =
    AccumT $ \ w -> runAccumT m w `catchE` \ e -> runAccumT (h e) w
{-# INLINE liftCatch #-}

-- | Lift a @listen@ operation to the new monad.
liftListen :: (Monad m) => Listen w m (a, s) -> Listen w (AccumT s m) a
liftListen listen m = AccumT $ \ s -> do
    ~((a, s'), w) <- listen (runAccumT m s)
    return ((a, w), s')
{-# INLINE liftListen #-}

-- | Lift a @pass@ operation to the new monad.
liftPass :: (Monad m) => Pass w m (a, s) -> Pass w (AccumT s m) a
liftPass pass m = AccumT $ \ s -> pass $ do
    ~((a, f), s') <- runAccumT m s
    return ((a, s'), f)
{-# INLINE liftPass #-}

-- | Convert a read-only computation into an accumulation computation.
readerToAccumT :: (Functor m, Monoid w) => ReaderT w m a -> AccumT w m a
readerToAccumT (ReaderT f) = AccumT $ \ w -> fmap (\ a -> (a, mempty)) (f w)
{-# INLINE readerToAccumT #-}

-- | Convert a writer computation into an accumulation computation.
writerToAccumT :: WriterT w m a -> AccumT w m a
writerToAccumT (WriterT m) = AccumT $ const $ m
{-# INLINE writerToAccumT #-}

-- | Convert an accumulation (append-only) computation into a fully
-- stateful computation.
accumToStateT :: (Functor m, Monoid s) => AccumT s m a -> StateT s m a
accumToStateT (AccumT f) =
    StateT $ \ w -> fmap (\ ~(a, w') -> (a, w `mappend` w')) (f w)
{-# INLINE accumToStateT #-}
