{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Maybe
-- Copyright   :  (c) 2007 Yitzak Gale, Eric Kidd
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The 'MaybeT' monad transformer extends a monad with the ability to exit
-- the computation without returning a value.
--
-- A sequence of actions produces a value only if all the actions in
-- the sequence do.  If one exits, the rest of the sequence is skipped
-- and the composite action exits.
--
-- For a variant allowing a range of exception values, see
-- "Control.Monad.Trans.Except".
-----------------------------------------------------------------------------

module Control.Monad.Trans.Maybe (
    -- * The MaybeT monad transformer
    MaybeT(..),
    mapMaybeT,
    -- * Monad transformations
    maybeToExceptT,
    exceptToMaybeT,
    -- * Lifting other operations
    liftCallCC,
    liftCatch,
    liftListen,
    liftPass,
  ) where

import Control.Monad.IO.Class
import Control.Monad.Signatures
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT(..))
import Data.Functor.Classes

import Control.Applicative
import Control.Monad (MonadPlus(mzero, mplus), liftM)
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif
import Control.Monad.Fix (MonadFix(mfix))
#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip (MonadZip(mzipWith))
#endif
import Data.Foldable (Foldable(foldMap))
import Data.Maybe (fromMaybe)
import Data.Traversable (Traversable(traverse))

-- | The parameterizable maybe monad, obtained by composing an arbitrary
-- monad with the 'Maybe' monad.
--
-- Computations are actions that may produce a value or exit.
--
-- The 'return' function yields a computation that produces that
-- value, while @>>=@ sequences two subcomputations, exiting if either
-- computation does.
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Eq1 m) => Eq1 (MaybeT m) where
    liftEq eq (MaybeT x) (MaybeT y) = liftEq (liftEq eq) x y
    {-# INLINE liftEq #-}

instance (Ord1 m) => Ord1 (MaybeT m) where
    liftCompare comp (MaybeT x) (MaybeT y) = liftCompare (liftCompare comp) x y
    {-# INLINE liftCompare #-}

instance (Read1 m) => Read1 (MaybeT m) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "MaybeT" MaybeT
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl

instance (Show1 m) => Show1 (MaybeT m) where
    liftShowsPrec sp sl d (MaybeT m) =
        showsUnaryWith (liftShowsPrec sp' sl') "MaybeT" d m
      where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList sp sl

instance (Eq1 m, Eq a) => Eq (MaybeT m a) where (==) = eq1
instance (Ord1 m, Ord a) => Ord (MaybeT m a) where compare = compare1
instance (Read1 m, Read a) => Read (MaybeT m a) where readsPrec = readsPrec1
instance (Show1 m, Show a) => Show (MaybeT m a) where showsPrec = showsPrec1

-- | Transform the computation inside a @MaybeT@.
--
-- * @'runMaybeT' ('mapMaybeT' f m) = f ('runMaybeT' m)@
mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f = MaybeT . f . runMaybeT
{-# INLINE mapMaybeT #-}

-- | Convert a 'MaybeT' computation to 'ExceptT', with a default
-- exception value.
maybeToExceptT :: (Functor m) => e -> MaybeT m a -> ExceptT e m a
maybeToExceptT e (MaybeT m) = ExceptT $ fmap (maybe (Left e) Right) m
{-# INLINE maybeToExceptT #-}

-- | Convert a 'ExceptT' computation to 'MaybeT', discarding the
-- value of any exception.
exceptToMaybeT :: (Functor m) => ExceptT e m a -> MaybeT m a
exceptToMaybeT (ExceptT m) = MaybeT $ fmap (either (const Nothing) Just) m
{-# INLINE exceptToMaybeT #-}

instance (Functor m) => Functor (MaybeT m) where
    fmap f = mapMaybeT (fmap (fmap f))
    {-# INLINE fmap #-}

instance (Foldable f) => Foldable (MaybeT f) where
    foldMap f (MaybeT a) = foldMap (foldMap f) a
    {-# INLINE foldMap #-}

instance (Traversable f) => Traversable (MaybeT f) where
    traverse f (MaybeT a) = MaybeT <$> traverse (traverse f) a
    {-# INLINE traverse #-}

instance (Functor m, Monad m) => Applicative (MaybeT m) where
    pure = MaybeT . return . Just
    {-# INLINE pure #-}
    mf <*> mx = MaybeT $ do
        mb_f <- runMaybeT mf
        case mb_f of
            Nothing -> return Nothing
            Just f  -> do
                mb_x <- runMaybeT mx
                case mb_x of
                    Nothing -> return Nothing
                    Just x  -> return (Just (f x))
    {-# INLINE (<*>) #-}

instance (Functor m, Monad m) => Alternative (MaybeT m) where
    empty = MaybeT (return Nothing)
    {-# INLINE empty #-}
    x <|> y = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> runMaybeT y
            Just _  -> return v
    {-# INLINE (<|>) #-}

instance (Monad m) => Monad (MaybeT m) where
#if !(MIN_VERSION_base(4,8,0))
    return = MaybeT . return . Just
    {-# INLINE return #-}
#endif
    x >>= f = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> return Nothing
            Just y  -> runMaybeT (f y)
    {-# INLINE (>>=) #-}
    fail _ = MaybeT (return Nothing)
    {-# INLINE fail #-}

#if MIN_VERSION_base(4,9,0)
instance (Monad m) => Fail.MonadFail (MaybeT m) where
    fail _ = MaybeT (return Nothing)
    {-# INLINE fail #-}
#endif

instance (Monad m) => MonadPlus (MaybeT m) where
    mzero = MaybeT (return Nothing)
    {-# INLINE mzero #-}
    mplus x y = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> runMaybeT y
            Just _  -> return v
    {-# INLINE mplus #-}

instance (MonadFix m) => MonadFix (MaybeT m) where
    mfix f = MaybeT (mfix (runMaybeT . f . fromMaybe bomb))
      where bomb = error "mfix (MaybeT): inner computation returned Nothing"
    {-# INLINE mfix #-}

instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just
    {-# INLINE lift #-}

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

#if MIN_VERSION_base(4,4,0)
instance (MonadZip m) => MonadZip (MaybeT m) where
    mzipWith f (MaybeT a) (MaybeT b) = MaybeT $ mzipWith (liftA2 f) a b
    {-# INLINE mzipWith #-}
#endif

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: CallCC m (Maybe a) (Maybe b) -> CallCC (MaybeT m) a b
liftCallCC callCC f =
    MaybeT $ callCC $ \ c -> runMaybeT (f (MaybeT . c . Just))
{-# INLINE liftCallCC #-}

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: Catch e m (Maybe a) -> Catch e (MaybeT m) a
liftCatch f m h = MaybeT $ f (runMaybeT m) (runMaybeT . h)
{-# INLINE liftCatch #-}

-- | Lift a @listen@ operation to the new monad.
liftListen :: (Monad m) => Listen w m (Maybe a) -> Listen w (MaybeT m) a
liftListen listen = mapMaybeT $ \ m -> do
    (a, w) <- listen m
    return $! fmap (\ r -> (r, w)) a
{-# INLINE liftListen #-}

-- | Lift a @pass@ operation to the new monad.
liftPass :: (Monad m) => Pass w m (Maybe a) -> Pass w (MaybeT m) a
liftPass pass = mapMaybeT $ \ m -> pass $ do
    a <- m
    return $! case a of
        Nothing     -> (Nothing, id)
        Just (v, f) -> (Just v, f)
{-# INLINE liftPass #-}
