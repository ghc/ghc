{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Writer.Strict
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The strict 'WriterT' monad transformer, which adds collection of
-- outputs (such as a count or string output) to a given monad.
--
-- This monad transformer provides only limited access to the output
-- during the computation.  For more general access, use
-- "Control.Monad.Trans.State" instead.
--
-- This version builds its output strictly; for a lazy version with
-- the same interface, see "Control.Monad.Trans.Writer.Lazy".
-- Although the output is built strictly, it is not possible to
-- achieve constant space behaviour with this transformer: for that,
-- use "Control.Monad.Trans.State.Strict" instead.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Writer.Strict (
    -- * The Writer monad
    Writer,
    writer,
    runWriter,
    execWriter,
    mapWriter,
    -- * The WriterT monad transformer
    WriterT(..),
    execWriterT,
    mapWriterT,
    -- * Writer operations
    tell,
    listen,
    listens,
    pass,
    censor,
    -- * Lifting other operations
    liftCallCC,
    liftCatch,
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Classes
import Data.Functor.Identity

import Control.Applicative
import Control.Monad
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif
import Control.Monad.Fix
import Control.Monad.Signatures
#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip (MonadZip(mzipWith))
#endif
import Data.Foldable
import Data.Monoid
import Data.Traversable (Traversable(traverse))
import Prelude hiding (null, length)

-- ---------------------------------------------------------------------------
-- | A writer monad parameterized by the type @w@ of output to accumulate.
--
-- The 'return' function produces the output 'mempty', while @>>=@
-- combines the outputs of the subcomputations using 'mappend'.
type Writer w = WriterT w Identity

-- | Construct a writer computation from a (result, output) pair.
-- (The inverse of 'runWriter'.)
writer :: (Monad m) => (a, w) -> WriterT w m a
writer = WriterT . return
{-# INLINE writer #-}

-- | Unwrap a writer computation as a (result, output) pair.
-- (The inverse of 'writer'.)
runWriter :: Writer w a -> (a, w)
runWriter = runIdentity . runWriterT
{-# INLINE runWriter #-}

-- | Extract the output from a writer computation.
--
-- * @'execWriter' m = 'snd' ('runWriter' m)@
execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)
{-# INLINE execWriter #-}

-- | Map both the return value and output of a computation using
-- the given function.
--
-- * @'runWriter' ('mapWriter' f m) = f ('runWriter' m)@
mapWriter :: ((a, w) -> (b, w')) -> Writer w a -> Writer w' b
mapWriter f = mapWriterT (Identity . f . runIdentity)
{-# INLINE mapWriter #-}

-- ---------------------------------------------------------------------------
-- | A writer monad parameterized by:
--
--   * @w@ - the output to accumulate.
--
--   * @m@ - The inner monad.
--
-- The 'return' function produces the output 'mempty', while @>>=@
-- combines the outputs of the subcomputations using 'mappend'.
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

instance (Eq w, Eq1 m) => Eq1 (WriterT w m) where
    liftEq eq (WriterT m1) (WriterT m2) = liftEq (liftEq2 eq (==)) m1 m2
    {-# INLINE liftEq #-}

instance (Ord w, Ord1 m) => Ord1 (WriterT w m) where
    liftCompare comp (WriterT m1) (WriterT m2) =
        liftCompare (liftCompare2 comp compare) m1 m2
    {-# INLINE liftCompare #-}

instance (Read w, Read1 m) => Read1 (WriterT w m) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "WriterT" WriterT
      where
        rp' = liftReadsPrec2 rp rl readsPrec readList
        rl' = liftReadList2 rp rl readsPrec readList

instance (Show w, Show1 m) => Show1 (WriterT w m) where
    liftShowsPrec sp sl d (WriterT m) =
        showsUnaryWith (liftShowsPrec sp' sl') "WriterT" d m
      where
        sp' = liftShowsPrec2 sp sl showsPrec showList
        sl' = liftShowList2 sp sl showsPrec showList

instance (Eq w, Eq1 m, Eq a) => Eq (WriterT w m a) where (==) = eq1
instance (Ord w, Ord1 m, Ord a) => Ord (WriterT w m a) where compare = compare1
instance (Read w, Read1 m, Read a) => Read (WriterT w m a) where
    readsPrec = readsPrec1
instance (Show w, Show1 m, Show a) => Show (WriterT w m a) where
    showsPrec = showsPrec1

-- | Extract the output from a writer computation.
--
-- * @'execWriterT' m = 'liftM' 'snd' ('runWriterT' m)@
execWriterT :: (Monad m) => WriterT w m a -> m w
execWriterT m = do
    (_, w) <- runWriterT m
    return w
{-# INLINE execWriterT #-}

-- | Map both the return value and output of a computation using
-- the given function.
--
-- * @'runWriterT' ('mapWriterT' f m) = f ('runWriterT' m)@
mapWriterT :: (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f m = WriterT $ f (runWriterT m)
{-# INLINE mapWriterT #-}

instance (Functor m) => Functor (WriterT w m) where
    fmap f = mapWriterT $ fmap $ \ (a, w) -> (f a, w)
    {-# INLINE fmap #-}

instance (Foldable f) => Foldable (WriterT w f) where
    foldMap f = foldMap (f . fst) . runWriterT
    {-# INLINE foldMap #-}
#if MIN_VERSION_base(4,8,0)
    null (WriterT t) = null t
    length (WriterT t) = length t
#endif

instance (Traversable f) => Traversable (WriterT w f) where
    traverse f = fmap WriterT . traverse f' . runWriterT where
       f' (a, b) = fmap (\ c -> (c, b)) (f a)
    {-# INLINE traverse #-}

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
    pure a  = WriterT $ pure (a, mempty)
    {-# INLINE pure #-}
    f <*> v = WriterT $ liftA2 k (runWriterT f) (runWriterT v)
      where k (a, w) (b, w') = (a b, w `mappend` w')
    {-# INLINE (<*>) #-}

instance (Monoid w, Alternative m) => Alternative (WriterT w m) where
    empty   = WriterT empty
    {-# INLINE empty #-}
    m <|> n = WriterT $ runWriterT m <|> runWriterT n
    {-# INLINE (<|>) #-}

instance (Monoid w, Monad m) => Monad (WriterT w m) where
#if !(MIN_VERSION_base(4,8,0))
    return a = writer (a, mempty)
    {-# INLINE return #-}
#endif
    m >>= k  = WriterT $ do
        (a, w)  <- runWriterT m
        (b, w') <- runWriterT (k a)
        return (b, w `mappend` w')
    {-# INLINE (>>=) #-}
    fail msg = WriterT $ fail msg
    {-# INLINE fail #-}

#if MIN_VERSION_base(4,9,0)
instance (Monoid w, Fail.MonadFail m) => Fail.MonadFail (WriterT w m) where
    fail msg = WriterT $ Fail.fail msg
    {-# INLINE fail #-}
#endif

instance (Monoid w, MonadPlus m) => MonadPlus (WriterT w m) where
    mzero       = WriterT mzero
    {-# INLINE mzero #-}
    m `mplus` n = WriterT $ runWriterT m `mplus` runWriterT n
    {-# INLINE mplus #-}

instance (Monoid w, MonadFix m) => MonadFix (WriterT w m) where
    mfix m = WriterT $ mfix $ \ ~(a, _) -> runWriterT (m a)
    {-# INLINE mfix #-}

instance (Monoid w) => MonadTrans (WriterT w) where
    lift m = WriterT $ do
        a <- m
        return (a, mempty)
    {-# INLINE lift #-}

instance (Monoid w, MonadIO m) => MonadIO (WriterT w m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

#if MIN_VERSION_base(4,4,0)
instance (Monoid w, MonadZip m) => MonadZip (WriterT w m) where
    mzipWith f (WriterT x) (WriterT y) = WriterT $
        mzipWith (\ (a, w) (b, w') -> (f a b, w `mappend` w')) x y
    {-# INLINE mzipWith #-}
#endif

-- | @'tell' w@ is an action that produces the output @w@.
tell :: (Monad m) => w -> WriterT w m ()
tell w = writer ((), w)
{-# INLINE tell #-}

-- | @'listen' m@ is an action that executes the action @m@ and adds its
-- output to the value of the computation.
--
-- * @'runWriterT' ('listen' m) = 'liftM' (\\ (a, w) -> ((a, w), w)) ('runWriterT' m)@
listen :: (Monad m) => WriterT w m a -> WriterT w m (a, w)
listen m = WriterT $ do
    (a, w) <- runWriterT m
    return ((a, w), w)
{-# INLINE listen #-}

-- | @'listens' f m@ is an action that executes the action @m@ and adds
-- the result of applying @f@ to the output to the value of the computation.
--
-- * @'listens' f m = 'liftM' (id *** f) ('listen' m)@
--
-- * @'runWriterT' ('listens' f m) = 'liftM' (\\ (a, w) -> ((a, f w), w)) ('runWriterT' m)@
listens :: (Monad m) => (w -> b) -> WriterT w m a -> WriterT w m (a, b)
listens f m = WriterT $ do
    (a, w) <- runWriterT m
    return ((a, f w), w)
{-# INLINE listens #-}

-- | @'pass' m@ is an action that executes the action @m@, which returns
-- a value and a function, and returns the value, applying the function
-- to the output.
--
-- * @'runWriterT' ('pass' m) = 'liftM' (\\ ((a, f), w) -> (a, f w)) ('runWriterT' m)@
pass :: (Monad m) => WriterT w m (a, w -> w) -> WriterT w m a
pass m = WriterT $ do
    ((a, f), w) <- runWriterT m
    return (a, f w)
{-# INLINE pass #-}

-- | @'censor' f m@ is an action that executes the action @m@ and
-- applies the function @f@ to its output, leaving the return value
-- unchanged.
--
-- * @'censor' f m = 'pass' ('liftM' (\\ x -> (x,f)) m)@
--
-- * @'runWriterT' ('censor' f m) = 'liftM' (\\ (a, w) -> (a, f w)) ('runWriterT' m)@
censor :: (Monad m) => (w -> w) -> WriterT w m a -> WriterT w m a
censor f m = WriterT $ do
    (a, w) <- runWriterT m
    return (a, f w)
{-# INLINE censor #-}

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: (Monoid w) => CallCC m (a,w) (b,w) -> CallCC (WriterT w m) a b
liftCallCC callCC f = WriterT $
    callCC $ \ c ->
    runWriterT (f (\ a -> WriterT $ c (a, mempty)))
{-# INLINE liftCallCC #-}

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: Catch e m (a,w) -> Catch e (WriterT w m) a
liftCatch catchE m h =
    WriterT $ runWriterT m `catchE` \ e -> runWriterT (h e)
{-# INLINE liftCatch #-}
