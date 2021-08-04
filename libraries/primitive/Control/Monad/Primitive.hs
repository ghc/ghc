{-# LANGUAGE CPP, MagicHash, UnboxedTuples, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- |
-- Module      : Control.Monad.Primitive
-- Copyright   : (c) Roman Leshchinskiy 2009
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Primitive state-transformer monads
--

module Control.Monad.Primitive (
  PrimMonad(..), RealWorld, primitive_,
  PrimBase(..),
  MonadPrim,
  MonadPrimBase,
  liftPrim, primToPrim, primToIO, primToST, ioToPrim, stToPrim,
  unsafePrimToPrim, unsafePrimToIO, unsafePrimToST, unsafeIOToPrim,
  unsafeSTToPrim, unsafeInlinePrim, unsafeInlineIO, unsafeInlineST,
  touch, evalPrim, unsafeInterleave, unsafeDupableInterleave, noDuplicate
) where

import GHC.Exts   ( State#, RealWorld, noDuplicate#, touch#
                  , unsafeCoerce#, realWorld#, seq# )
import GHC.IO     ( IO(..) )
import GHC.ST     ( ST(..) )

import qualified Control.Monad.ST.Lazy as L

import Control.Monad.Trans.Class (lift)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid)
#endif

import Control.Monad.Trans.Cont     ( ContT    )
import Control.Monad.Trans.Identity ( IdentityT (IdentityT) )
import Control.Monad.Trans.List     ( ListT    )
import Control.Monad.Trans.Maybe    ( MaybeT   )
import Control.Monad.Trans.Error    ( ErrorT, Error)
import Control.Monad.Trans.Reader   ( ReaderT  )
import Control.Monad.Trans.State    ( StateT   )
import Control.Monad.Trans.Writer   ( WriterT  )
import Control.Monad.Trans.RWS      ( RWST     )

#if MIN_VERSION_transformers(0,4,0)
import Control.Monad.Trans.Except   ( ExceptT  )
#endif

#if MIN_VERSION_transformers(0,5,3)
import Control.Monad.Trans.Accum    ( AccumT   )
import Control.Monad.Trans.Select   ( SelectT  )
#endif

#if MIN_VERSION_transformers(0,5,6)
import qualified Control.Monad.Trans.Writer.CPS as CPS
import qualified Control.Monad.Trans.RWS.CPS as CPS
#endif

import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )

-- | Class of monads which can perform primitive state-transformer actions
class Monad m => PrimMonad m where
  -- | State token type
  type PrimState m

  -- | Execute a primitive operation
  primitive :: (State# (PrimState m) -> (# State# (PrimState m), a #)) -> m a

-- | Class of primitive monads for state-transformer actions.
--
-- Unlike 'PrimMonad', this typeclass requires that the @Monad@ be fully
-- expressed as a state transformer, therefore disallowing other monad
-- transformers on top of the base @IO@ or @ST@.
--
-- @since 0.6.0.0
class PrimMonad m => PrimBase m where
  -- | Expose the internal structure of the monad
  internal :: m a -> State# (PrimState m) -> (# State# (PrimState m), a #)

-- | Execute a primitive operation with no result
primitive_ :: PrimMonad m
              => (State# (PrimState m) -> State# (PrimState m)) -> m ()
{-# INLINE primitive_ #-}
primitive_ f = primitive (\s# ->
    case f s# of
        s'# -> (# s'#, () #))

instance PrimMonad IO where
  type PrimState IO = RealWorld
  primitive = IO
  {-# INLINE primitive #-}
instance PrimBase IO where
  internal (IO p) = p
  {-# INLINE internal #-}

-- | @since 0.6.3.0
instance PrimMonad m => PrimMonad (ContT r m) where
  type PrimState (ContT r m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance PrimMonad m => PrimMonad (IdentityT m) where
  type PrimState (IdentityT m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

-- | @since 0.6.2.0
instance PrimBase m => PrimBase (IdentityT m) where
  internal (IdentityT m) = internal m
  {-# INLINE internal #-}

instance PrimMonad m => PrimMonad (ListT m) where
  type PrimState (ListT m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance PrimMonad m => PrimMonad (MaybeT m) where
  type PrimState (MaybeT m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance (Error e, PrimMonad m) => PrimMonad (ErrorT e m) where
  type PrimState (ErrorT e m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance PrimMonad m => PrimMonad (ReaderT r m) where
  type PrimState (ReaderT r m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance PrimMonad m => PrimMonad (StateT s m) where
  type PrimState (StateT s m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance (Monoid w, PrimMonad m) => PrimMonad (WriterT w m) where
  type PrimState (WriterT w m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

#if MIN_VERSION_transformers(0,5,6)
instance (Monoid w, PrimMonad m) => PrimMonad (CPS.WriterT w m) where
  type PrimState (CPS.WriterT w m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}
#endif

instance (Monoid w, PrimMonad m) => PrimMonad (RWST r w s m) where
  type PrimState (RWST r w s m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

#if MIN_VERSION_transformers(0,5,6)
instance (Monoid w, PrimMonad m) => PrimMonad (CPS.RWST r w s m) where
  type PrimState (CPS.RWST r w s m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}
#endif

#if MIN_VERSION_transformers(0,4,0)
instance PrimMonad m => PrimMonad (ExceptT e m) where
  type PrimState (ExceptT e m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}
#endif

#if MIN_VERSION_transformers(0,5,3)
-- | @since 0.6.3.0
instance ( Monoid w
         , PrimMonad m
# if !(MIN_VERSION_base(4,8,0))
         , Functor m
# endif
         ) => PrimMonad (AccumT w m) where
  type PrimState (AccumT w m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}
instance PrimMonad m => PrimMonad (SelectT r m) where
  type PrimState (SelectT r m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}
#endif

instance PrimMonad m => PrimMonad (Strict.StateT s m) where
  type PrimState (Strict.StateT s m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance (Monoid w, PrimMonad m) => PrimMonad (Strict.WriterT w m) where
  type PrimState (Strict.WriterT w m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance (Monoid w, PrimMonad m) => PrimMonad (Strict.RWST r w s m) where
  type PrimState (Strict.RWST r w s m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance PrimMonad (ST s) where
  type PrimState (ST s) = s
  primitive = ST
  {-# INLINE primitive #-}
instance PrimBase (ST s) where
  internal (ST p) = p
  {-# INLINE internal #-}

-- see https://gitlab.haskell.org/ghc/ghc/commit/2f5cb3d44d05e581b75a47fec222577dfa7a533e
-- for why we only support an instance for ghc >= 8.2
#if __GLASGOW_HASKELL__ >= 802
-- @since 0.7.1.0
instance PrimMonad (L.ST s) where
  type PrimState (L.ST s) = s
  primitive = L.strictToLazyST . primitive
  {-# INLINE primitive #-}

-- @since 0.7.1.0
instance PrimBase (L.ST s) where
  internal = internal . L.lazyToStrictST
  {-# INLINE internal #-}
#endif

-- | 'PrimMonad''s state token type can be annoying to handle
--   in constraints. This typeclass lets users (visually) notice
--   'PrimState' equality constraints less, by witnessing that
--   @s ~ 'PrimState' m@.
class (PrimMonad m, s ~ PrimState m) => MonadPrim s m
instance (PrimMonad m, s ~ PrimState m) => MonadPrim s m

-- | 'PrimBase''s state token type can be annoying to handle
--   in constraints. This typeclass lets users (visually) notice
--   'PrimState' equality constraints less, by witnessing that
--   @s ~ 'PrimState' m@.
class (PrimBase m, MonadPrim s m) => MonadPrimBase s m
instance (PrimBase m, MonadPrim s m) => MonadPrimBase s m

-- | Lifts a 'PrimBase' into another 'PrimMonad' with the same underlying state
-- token type.
liftPrim
  :: (PrimBase m1, PrimMonad m2, PrimState m1 ~ PrimState m2) => m1 a -> m2 a
{-# INLINE liftPrim #-}
liftPrim = primToPrim

-- | Convert a 'PrimBase' to another monad with the same state token.
primToPrim :: (PrimBase m1, PrimMonad m2, PrimState m1 ~ PrimState m2)
        => m1 a -> m2 a
{-# INLINE primToPrim #-}
primToPrim m = primitive (internal m)

-- | Convert a 'PrimBase' with a 'RealWorld' state token to 'IO'
primToIO :: (PrimBase m, PrimState m ~ RealWorld) => m a -> IO a
{-# INLINE primToIO #-}
primToIO = primToPrim

-- | Convert a 'PrimBase' to 'ST'
primToST :: PrimBase m => m a -> ST (PrimState m) a
{-# INLINE primToST #-}
primToST = primToPrim

-- | Convert an 'IO' action to a 'PrimMonad'.
--
-- @since 0.6.2.0
ioToPrim :: (PrimMonad m, PrimState m ~ RealWorld) => IO a -> m a
{-# INLINE ioToPrim #-}
ioToPrim = primToPrim

-- | Convert an 'ST' action to a 'PrimMonad'.
--
-- @since 0.6.2.0
stToPrim :: PrimMonad m => ST (PrimState m) a -> m a
{-# INLINE stToPrim #-}
stToPrim = primToPrim

-- | Convert a 'PrimBase' to another monad with a possibly different state
-- token. This operation is highly unsafe!
unsafePrimToPrim :: (PrimBase m1, PrimMonad m2) => m1 a -> m2 a
{-# INLINE unsafePrimToPrim #-}
unsafePrimToPrim m = primitive (unsafeCoerce# (internal m))

-- | Convert any 'PrimBase' to 'ST' with an arbitrary state token. This
-- operation is highly unsafe!
unsafePrimToST :: PrimBase m => m a -> ST s a
{-# INLINE unsafePrimToST #-}
unsafePrimToST = unsafePrimToPrim

-- | Convert any 'PrimBase' to 'IO'. This operation is highly unsafe!
unsafePrimToIO :: PrimBase m => m a -> IO a
{-# INLINE unsafePrimToIO #-}
unsafePrimToIO = unsafePrimToPrim

-- | Convert an 'ST' action with an arbitrary state token to any 'PrimMonad'.
-- This operation is highly unsafe!
--
-- @since 0.6.2.0
unsafeSTToPrim :: PrimMonad m => ST s a -> m a
{-# INLINE unsafeSTToPrim #-}
unsafeSTToPrim = unsafePrimToPrim

-- | Convert an 'IO' action to any 'PrimMonad'. This operation is highly
-- unsafe!
--
-- @since 0.6.2.0
unsafeIOToPrim :: PrimMonad m => IO a -> m a
{-# INLINE unsafeIOToPrim #-}
unsafeIOToPrim = unsafePrimToPrim

-- | See 'unsafeInlineIO'. This function is not recommended for the same
-- reasons.
unsafeInlinePrim :: PrimBase m => m a -> a
{-# INLINE unsafeInlinePrim #-}
unsafeInlinePrim m = unsafeInlineIO (unsafePrimToIO m)

-- | Generally, do not use this function. It is the same as
-- @accursedUnutterablePerformIO@ from @bytestring@ and is well behaved under
-- narrow conditions. See the documentation of that function to get an idea
-- of when this is sound. In most cases @GHC.IO.Unsafe.unsafeDupablePerformIO@
-- should be preferred.
unsafeInlineIO :: IO a -> a
{-# INLINE unsafeInlineIO #-}
unsafeInlineIO m = case internal m realWorld# of (# _, r #) -> r

-- | See 'unsafeInlineIO'. This function is not recommended for the same
-- reasons. Prefer @runST@ when @s@ is free.
unsafeInlineST :: ST s a -> a
{-# INLINE unsafeInlineST #-}
unsafeInlineST = unsafeInlinePrim

touch :: PrimMonad m => a -> m ()
{-# INLINE touch #-}
touch x = unsafePrimToPrim
        $ (primitive (\s -> case touch# x s of { s' -> (# s', () #) }) :: IO ())

-- | Create an action to force a value; generalizes 'Control.Exception.evaluate'
--
-- @since 0.6.2.0
evalPrim :: forall a m . PrimMonad m => a -> m a
#if MIN_VERSION_base(4,4,0)
evalPrim a = primitive (\s -> seq# a s)
#else
-- This may or may not work so well, but there's probably nothing better to do.
{-# NOINLINE evalPrim #-}
evalPrim a = unsafePrimToPrim (evaluate a :: IO a)
#endif

noDuplicate :: PrimMonad m => m ()
#if __GLASGOW_HASKELL__ >= 802
noDuplicate = primitive $ \ s -> (# noDuplicate# s, () #)
#else
-- noDuplicate# was limited to RealWorld
noDuplicate = unsafeIOToPrim $ primitive $ \s -> (# noDuplicate# s, () #)
#endif

unsafeInterleave, unsafeDupableInterleave :: PrimBase m => m a -> m a
unsafeInterleave x = unsafeDupableInterleave (noDuplicate >> x)
unsafeDupableInterleave x = primitive $ \ s -> let r' = case internal x s of (# _, r #) -> r in (# s, r' #)
{-# INLINE unsafeInterleave #-}
{-# NOINLINE unsafeDupableInterleave #-}
-- See Note [unsafeDupableInterleaveIO should not be inlined]
-- in GHC.IO.Unsafe
