{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Identity
-- Copyright   :  (c) 2007 Magnus Therning
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The identity monad transformer.
--
-- This is useful for functions parameterized by a monad transformer.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Identity (
    -- * The identity monad transformer
    IdentityT(..),
    mapIdentityT,
    -- * Lifting other operations
    liftCatch,
    liftCallCC,
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Signatures
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Functor.Classes

import Control.Applicative
import Control.Monad (MonadPlus(mzero, mplus))
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif
import Control.Monad.Fix (MonadFix(mfix))
#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip (MonadZip(mzipWith))
#endif
import Data.Foldable
import Data.Traversable (Traversable(traverse))
import Prelude hiding (foldr, foldr1, foldl, foldl1, null, length)

-- | The trivial monad transformer, which maps a monad to an equivalent monad.
newtype IdentityT f a = IdentityT { runIdentityT :: f a }

instance (Eq1 f) => Eq1 (IdentityT f) where
    liftEq eq (IdentityT x) (IdentityT y) = liftEq eq x y
    {-# INLINE liftEq #-}

instance (Ord1 f) => Ord1 (IdentityT f) where
    liftCompare comp (IdentityT x) (IdentityT y) = liftCompare comp x y
    {-# INLINE liftCompare #-}

instance (Read1 f) => Read1 (IdentityT f) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp rl) "IdentityT" IdentityT

instance (Show1 f) => Show1 (IdentityT f) where
    liftShowsPrec sp sl d (IdentityT m) =
        showsUnaryWith (liftShowsPrec sp sl) "IdentityT" d m

instance (Eq1 f, Eq a) => Eq (IdentityT f a) where (==) = eq1
instance (Ord1 f, Ord a) => Ord (IdentityT f a) where compare = compare1
instance (Read1 f, Read a) => Read (IdentityT f a) where readsPrec = readsPrec1
instance (Show1 f, Show a) => Show (IdentityT f a) where showsPrec = showsPrec1

instance (Functor m) => Functor (IdentityT m) where
    fmap f = mapIdentityT (fmap f)
    {-# INLINE fmap #-}

instance (Foldable f) => Foldable (IdentityT f) where
    foldMap f (IdentityT t) = foldMap f t
    {-# INLINE foldMap #-}
    foldr f z (IdentityT t) = foldr f z t
    {-# INLINE foldr #-}
    foldl f z (IdentityT t) = foldl f z t
    {-# INLINE foldl #-}
    foldr1 f (IdentityT t) = foldr1 f t
    {-# INLINE foldr1 #-}
    foldl1 f (IdentityT t) = foldl1 f t
    {-# INLINE foldl1 #-}
#if MIN_VERSION_base(4,8,0)
    null (IdentityT t) = null t
    length (IdentityT t) = length t
#endif

instance (Traversable f) => Traversable (IdentityT f) where
    traverse f (IdentityT a) = IdentityT <$> traverse f a
    {-# INLINE traverse #-}

instance (Applicative m) => Applicative (IdentityT m) where
    pure x = IdentityT (pure x)
    {-# INLINE pure #-}
    (<*>) = lift2IdentityT (<*>)
    {-# INLINE (<*>) #-}

instance (Alternative m) => Alternative (IdentityT m) where
    empty = IdentityT empty
    {-# INLINE empty #-}
    (<|>) = lift2IdentityT (<|>)
    {-# INLINE (<|>) #-}

instance (Monad m) => Monad (IdentityT m) where
#if !(MIN_VERSION_base(4,8,0))
    return = IdentityT . return
    {-# INLINE return #-}
#endif
    m >>= k = IdentityT $ runIdentityT . k =<< runIdentityT m
    {-# INLINE (>>=) #-}
    fail msg = IdentityT $ fail msg
    {-# INLINE fail #-}

#if MIN_VERSION_base(4,9,0)
instance (Fail.MonadFail m) => Fail.MonadFail (IdentityT m) where
    fail msg = IdentityT $ Fail.fail msg
    {-# INLINE fail #-}
#endif

instance (MonadPlus m) => MonadPlus (IdentityT m) where
    mzero = IdentityT mzero
    {-# INLINE mzero #-}
    mplus = lift2IdentityT mplus
    {-# INLINE mplus #-}

instance (MonadFix m) => MonadFix (IdentityT m) where
    mfix f = IdentityT (mfix (runIdentityT . f))
    {-# INLINE mfix #-}

instance (MonadIO m) => MonadIO (IdentityT m) where
    liftIO = IdentityT . liftIO
    {-# INLINE liftIO #-}

#if MIN_VERSION_base(4,4,0)
instance (MonadZip m) => MonadZip (IdentityT m) where
    mzipWith f = lift2IdentityT (mzipWith f)
    {-# INLINE mzipWith #-}
#endif

instance MonadTrans IdentityT where
    lift = IdentityT
    {-# INLINE lift #-}

-- | Lift a unary operation to the new monad.
mapIdentityT :: (m a -> n b) -> IdentityT m a -> IdentityT n b
mapIdentityT f = IdentityT . f . runIdentityT
{-# INLINE mapIdentityT #-}

-- | Lift a binary operation to the new monad.
lift2IdentityT ::
    (m a -> n b -> p c) -> IdentityT m a -> IdentityT n b -> IdentityT p c
lift2IdentityT f a b = IdentityT (f (runIdentityT a) (runIdentityT b))
{-# INLINE lift2IdentityT #-}

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: CallCC m a b -> CallCC (IdentityT m) a b
liftCallCC callCC f =
    IdentityT $ callCC $ \ c -> runIdentityT (f (IdentityT . c))
{-# INLINE liftCallCC #-}

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: Catch e m a -> Catch e (IdentityT m) a
liftCatch f m h = IdentityT $ f (runIdentityT m) (runIdentityT . h)
{-# INLINE liftCatch #-}
