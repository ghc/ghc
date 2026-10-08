{-# LANGUAGE PatternSynonyms #-}

-- | A reader monad transformer that is strict in its environment,
-- and uses the one-shot trick.
-- See Note [Instances for StrictReaderT]
module GHC.Utils.Monad.StrictReader
  ( StrictReaderT(StrictReaderT, runStrictReaderT)
  ) where

import GHC.Prelude

import GHC.Exts                  ( oneShot )

import Control.Monad.Fix         ( MonadFix(..) )
import Control.Monad.IO.Class    ( MonadIO(..) )
import Control.Monad.Trans.Class ( MonadTrans(..) )

-- | A reader monad transformer, strict in the environment.
-- See Note [Instances for StrictReaderT]
newtype StrictReaderT r m a = StrictReaderT' { runStrictReaderT :: r -> m a }

{- Note [Instances for StrictReaderT]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The instances for StrictReaderT (Functor, Applicative, Monad, MonadIO,
MonadTrans, MonadFix) are written by hand.  They behave just like the
instances for `ReaderT r m`, but differ in two ways:

1. They use oneShot. See Note [The one-shot state monad trick] in GHC.Utils.Monad.

2. They are strict in the environment. This allows us to worker-wrapper functions,
   passing them individual fields of the environment instead of the whole record.
   When this happens, we avoid allocating the environment record, which is a win.

That is why we cannot simply use `deriving via ReaderT r m`.

Both properties come from the builder of the pattern synonym `StrictReaderT`,
which is a smart constructor:
    StrictReaderT m = StrictReaderT' (oneShot (\ !env -> m env))
All StrictReaderT computations, whether built by the instances below or by
clients, are constructed with it, so they are all strict in the environment.
Clients do not need to add bangs themselves.  For example, in the substitution
mapper in GHC.Core.TyCo.Subst we have
    subst_ch ch = StrictReaderT $ \subst -> return (substCoHole subst ch)
Even though substCoHole is not strict in `subst`, the builder makes `subst_ch`
strict, and hence mapTyCo's `go_co` is strict in the Subst in every branch.

Only the builder adds oneShot and the bang: do not use StrictReaderT' directly.
(It is not exported.)

StrictReaderT is used for
  * ZonkT, in GHC.Tc.Zonk.Env
  * ZonkM, in GHC.Tc.Zonk.Monad
  * Substitution, via TyCoMapper; see GHC.Core.TyCo.Subst
-}

-- See Note [The one-shot state monad trick] in GHC.Utils.Monad
{-# COMPLETE StrictReaderT #-}
pattern StrictReaderT :: forall r m a. (r -> m a) -> StrictReaderT r m a
pattern StrictReaderT m <- StrictReaderT' m
  where
    StrictReaderT m = StrictReaderT' (oneShot (\ !env -> m env))

-- See Note [Instances for StrictReaderT]
instance Functor m => Functor (StrictReaderT r m) where
  fmap f (StrictReaderT g) = StrictReaderT $ \ env -> fmap f (g env)
  a <$ StrictReaderT g     = StrictReaderT $ \ env -> a <$ g env
  {-# INLINE fmap #-}
  {-# INLINE (<$) #-}

-- See Note [Instances for StrictReaderT]
instance Applicative m => Applicative (StrictReaderT r m) where
  pure a = StrictReaderT (\ _ -> pure a)
  StrictReaderT f <*> StrictReaderT x = StrictReaderT (\ env -> f env <*> x env )
  StrictReaderT m *> f = StrictReaderT (\ env -> m env *> runStrictReaderT f env)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}
  {-# INLINE (*>) #-}

-- See Note [Instances for StrictReaderT]
instance Monad m => Monad (StrictReaderT r m) where
  StrictReaderT m >>= f =
    StrictReaderT (\ env -> do { r <- m env
                                ; runStrictReaderT (f r) env })
  (>>)   = (*>)
  {-# INLINE (>>=) #-}
  {-# INLINE (>>) #-}

-- See Note [Instances for StrictReaderT]
instance MonadIO m => MonadIO (StrictReaderT r m) where
  liftIO f = StrictReaderT (\ _ -> liftIO f)
  {-# INLINE liftIO #-}

-- See Note [Instances for StrictReaderT]
instance MonadTrans (StrictReaderT r) where
  lift ma = StrictReaderT $ \ _ -> ma
  {-# INLINE lift #-}

-- See Note [Instances for StrictReaderT]
instance MonadFix m => MonadFix (StrictReaderT r m) where
  mfix f = StrictReaderT $ \ r -> mfix $ oneShot $ \ a -> runStrictReaderT (f a) r
  {-# INLINE mfix #-}
