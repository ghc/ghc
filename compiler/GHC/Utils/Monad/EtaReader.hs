{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Utils.Monad.EtaReader
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- This file is copied from "Control.Monad.Trans.Reader", but adds GHC-specific
-- annotations so that it always eta expands.
-- See Note [The one-shot state monad trick] in GHC.Utils.Monad.
--
-----------------------------------------------------------------------------

module GHC.Utils.Monad.EtaReader (
    -- * The EtaReader monad
    EtaReader,
    etaReader,
    runEtaReader,
    mapEtaReader,
    withEtaReader,
    -- * The EtaReaderT monad transformer
    EtaReaderT(NoEtaReaderT, EtaReaderT),
    runEtaReaderT,
    mapEtaReaderT,
    withEtaReaderT,
    -- * EtaReader operations
    ask,
    local,
    asks,
    -- * Lifting other operations
    liftCallCC,
    liftCatch,
    ) where

import Prelude

import Control.Monad.IO.Class
import Control.Monad.Signatures
import Control.Monad.Trans.Class
import Data.Functor.Contravariant
import Data.Functor.Identity

import Control.Applicative
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.Zip (MonadZip(mzipWith))
import GHC.Generics
import GHC.Exts (oneShot)

-- These aren't defined in `transformers`, but in `exceptions`:
import Control.Monad.Catch

-- | 'Control.Monad.Trans.Reader.Reader', but eta-expanded.
-- See Note [The one-shot state monad trick] in GHC.Utils.Monad.
type EtaReader r = EtaReaderT r Identity

-- | Constructor for computations in the etaReader monad (equivalent to 'asks').
etaReader :: (Monad m) => (r -> a) -> EtaReaderT r m a
etaReader f = EtaReaderT (return . f)
{-# INLINE etaReader #-}

-- | Runs a @EtaReader@ and extracts the final value from it.
-- (The inverse of 'etaReader'.)
runEtaReader
    :: EtaReader r a       -- ^ A @EtaReader@ to run.
    -> r                -- ^ An initial environment.
    -> a
runEtaReader m = runIdentity . runEtaReaderT m
{-# INLINE runEtaReader #-}

-- | Transform the value returned by a @EtaReader@.
--
-- * @'runEtaReader' ('mapEtaReader' f m) = f . 'runEtaReader' m@
mapEtaReader :: (a -> b) -> EtaReader r a -> EtaReader r b
mapEtaReader f = mapEtaReaderT (Identity . f . runIdentity)
{-# INLINE mapEtaReader #-}

-- | Execute a computation in a modified environment
-- (a specialization of 'withEtaReaderT').
--
-- * @'runEtaReader' ('withEtaReader' f m) = 'runEtaReader' m . f@
withEtaReader
    :: (r' -> r)        -- ^ The function to modify the environment.
    -> EtaReader r a       -- ^ Computation to run in the modified environment.
    -> EtaReader r' a
withEtaReader = withEtaReaderT
{-# INLINE withEtaReader #-}

-- | 'Control.Monad.Trans.Reader.ReaderT', but eta-expanded.
-- See Note [The one-shot state monad trick] in GHC.Utils.Monad.
newtype EtaReaderT r m a
  = NoEtaReaderT { runEtaReaderT :: r -> m a }
  -- ^ Using 'NoEtaReaderT' will *not* try to eagerly eta-expand the wrapped function.
  -- Sometimes this is desirable,
  deriving (Generic, Generic1)

-- This pattern synonym makes the monad eta-expand,
-- which as a very beneficial effect on compiler performance
-- See #18202.
-- See Note [The one-shot state monad trick] in GHC.Utils.Monad
pattern EtaReaderT :: (r -> m a) -> EtaReaderT r m a
pattern EtaReaderT m <- NoEtaReaderT m
  where
    EtaReaderT m = NoEtaReaderT (oneShot $ \r -> m r)
{-# COMPLETE EtaReaderT #-}

-- | Transform the computation inside a @EtaReaderT@.
--
-- * @'runEtaReaderT' ('mapEtaReaderT' f m) = f . 'runEtaReaderT' m@
mapEtaReaderT :: (m a -> n b) -> EtaReaderT r m a -> EtaReaderT r n b
mapEtaReaderT f m = EtaReaderT $ f . runEtaReaderT m
{-# INLINE mapEtaReaderT #-}

-- | Execute a computation in a modified environment
-- (a more general version of 'local').
--
-- * @'runEtaReaderT' ('withEtaReaderT' f m) = 'runEtaReaderT' m . f@
withEtaReaderT
    :: (r' -> r)        -- ^ The function to modify the environment.
    -> EtaReaderT r m a    -- ^ Computation to run in the modified environment.
    -> EtaReaderT r' m a
withEtaReaderT f m = EtaReaderT $ runEtaReaderT m . f
{-# INLINE withEtaReaderT #-}

instance (Functor m) => Functor (EtaReaderT r m) where
    fmap f  = mapEtaReaderT (fmap f)
    {-# INLINE fmap #-}
    x <$ v = mapEtaReaderT (x <$) v
    {-# INLINE (<$) #-}

instance (Applicative m) => Applicative (EtaReaderT r m) where
    pure    = liftEtaReaderT . pure
    {-# INLINE pure #-}
    f <*> v = EtaReaderT $ \ r -> runEtaReaderT f r <*> runEtaReaderT v r
    {-# INLINE (<*>) #-}
    u *> v = EtaReaderT $ \ r -> runEtaReaderT u r *> runEtaReaderT v r
    {-# INLINE (*>) #-}
    u <* v = EtaReaderT $ \ r -> runEtaReaderT u r <* runEtaReaderT v r
    {-# INLINE (<*) #-}
    liftA2 f x y = EtaReaderT $ \ r -> liftA2 f (runEtaReaderT x r) (runEtaReaderT y r)
    {-# INLINE liftA2 #-}

instance (Alternative m) => Alternative (EtaReaderT r m) where
    empty   = liftEtaReaderT empty
    {-# INLINE empty #-}
    m <|> n = EtaReaderT $ \ r -> runEtaReaderT m r <|> runEtaReaderT n r
    {-# INLINE (<|>) #-}

instance (Monad m) => Monad (EtaReaderT r m) where
    m >>= k  = EtaReaderT $ \ r -> do
        a <- runEtaReaderT m r
        runEtaReaderT (k a) r
    {-# INLINE (>>=) #-}
    (>>) = (*>)
    {-# INLINE (>>) #-}

instance (Fail.MonadFail m) => Fail.MonadFail (EtaReaderT r m) where
    fail msg = lift (Fail.fail msg)
    {-# INLINE fail #-}

instance (MonadPlus m) => MonadPlus (EtaReaderT r m) where
    mzero       = lift mzero
    {-# INLINE mzero #-}
    m `mplus` n = EtaReaderT $ \ r -> runEtaReaderT m r `mplus` runEtaReaderT n r
    {-# INLINE mplus #-}

instance (MonadFix m) => MonadFix (EtaReaderT r m) where
    mfix f = EtaReaderT $ \ r -> mfix $ \ a -> runEtaReaderT (f a) r
    {-# INLINE mfix #-}

instance MonadTrans (EtaReaderT r) where
    lift   = liftEtaReaderT
    {-# INLINE lift #-}

instance (MonadIO m) => MonadIO (EtaReaderT r m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance (MonadZip m) => MonadZip (EtaReaderT r m) where
    mzipWith f (EtaReaderT m) (EtaReaderT n) = EtaReaderT $ \ a ->
        mzipWith f (m a) (n a)
    {-# INLINE mzipWith #-}

instance Contravariant m => Contravariant (EtaReaderT r m) where
    contramap f = EtaReaderT . fmap (contramap f) . runEtaReaderT
    {-# INLINE contramap #-}

liftEtaReaderT :: m a -> EtaReaderT r m a
liftEtaReaderT m = EtaReaderT (const m)
{-# INLINE liftEtaReaderT #-}

-- | Fetch the value of the environment.
ask :: (Monad m) => EtaReaderT r m r
ask = EtaReaderT return
{-# INLINE ask #-}

-- | Execute a computation in a modified environment
-- (a specialization of 'withEtaReaderT').
--
-- * @'runEtaReaderT' ('local' f m) = 'runEtaReaderT' m . f@
local
    :: (r -> r)         -- ^ The function to modify the environment.
    -> EtaReaderT r m a    -- ^ Computation to run in the modified environment.
    -> EtaReaderT r m a
local = withEtaReaderT
{-# INLINE local #-}

-- | Retrieve a function of the current environment.
--
-- * @'asks' f = 'liftM' f 'ask'@
asks :: (Monad m)
    => (r -> a)         -- ^ The selector function to apply to the environment.
    -> EtaReaderT r m a
asks f = EtaReaderT (return . f)
{-# INLINE asks #-}

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: CallCC m a b -> CallCC (EtaReaderT r m) a b
liftCallCC callCC f = EtaReaderT $ \ r ->
    callCC $ \ c ->
    runEtaReaderT (f (EtaReaderT . const . c)) r
{-# INLINE liftCallCC #-}

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: Catch e m a -> Catch e (EtaReaderT r m) a
liftCatch f m h =
    EtaReaderT $ \ r -> f (runEtaReaderT m r) (\ e -> runEtaReaderT (h e) r)
{-# INLINE liftCatch #-}

-- From `exceptions`:
-- Note the lack of INLINE pragmas. I wanted to stay as close to the original
-- definitions as possible for the Eta* variant. If lack of eta-expansion
-- becomes a problem around these functions, add INLINE pragmas
instance MonadThrow m => MonadThrow (EtaReaderT r m) where
  throwM e = lift $ throwM e
instance MonadCatch m => MonadCatch (EtaReaderT r m) where
  catch (EtaReaderT m) c = EtaReaderT $ \r -> m r `catch` \e -> runEtaReaderT (c e) r
instance MonadMask m => MonadMask (EtaReaderT r m) where
  mask a = EtaReaderT $ \e -> mask $ \u -> runEtaReaderT (a $ q u) e
    where q :: (m a -> m a) -> EtaReaderT e m a -> EtaReaderT e m a
          q u (EtaReaderT b) = EtaReaderT (u . b)
  uninterruptibleMask a =
    EtaReaderT $ \e -> uninterruptibleMask $ \u -> runEtaReaderT (a $ q u) e
      where q :: (m a -> m a) -> EtaReaderT e m a -> EtaReaderT e m a
            q u (EtaReaderT b) = EtaReaderT (u . b)

  generalBracket acquire release use = EtaReaderT $ \r ->
    generalBracket
      (runEtaReaderT acquire r)
      (\resource exitCase -> runEtaReaderT (release resource exitCase) r)
      (\resource -> runEtaReaderT (use resource) r)
