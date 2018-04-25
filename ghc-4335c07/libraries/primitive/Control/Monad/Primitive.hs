{-# LANGUAGE CPP, MagicHash, UnboxedTuples, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  liftPrim, primToPrim, primToIO, primToST, ioToPrim, stToPrim,
  unsafePrimToPrim, unsafePrimToIO, unsafePrimToST, unsafeIOToPrim,
  unsafeSTToPrim, unsafeInlinePrim, unsafeInlineIO, unsafeInlineST,
  touch, evalPrim
) where

import GHC.Prim   ( State#, RealWorld, touch# )
import GHC.Base   ( unsafeCoerce#, realWorld# )
#if MIN_VERSION_base(4,4,0)
import GHC.Base   ( seq# )
#else
import Control.Exception (evaluate)
#endif
#if MIN_VERSION_base(4,2,0)
import GHC.IO     ( IO(..) )
#else
import GHC.IOBase ( IO(..) )
#endif
import GHC.ST     ( ST(..) )

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

instance PrimMonad m => PrimMonad (ContT r m) where
  type PrimState (ContT r m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}
instance PrimMonad m => PrimMonad (IdentityT m) where
  type PrimState (IdentityT m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}
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
instance (Monoid w, PrimMonad m) => PrimMonad (RWST r w s m) where
  type PrimState (RWST r w s m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

#if MIN_VERSION_transformers(0,4,0)
instance PrimMonad m => PrimMonad (ExceptT e m) where
  type PrimState (ExceptT e m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}
#endif

#if MIN_VERSION_transformers(0,5,3)
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
ioToPrim :: (PrimMonad m, PrimState m ~ RealWorld) => IO a -> m a
{-# INLINE ioToPrim #-}
ioToPrim = primToPrim

-- | Convert an 'ST' action to a 'PrimMonad'.
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

-- | Convert an 'ST' action with an arbitraty state token to any 'PrimMonad'.
-- This operation is highly unsafe!
unsafeSTToPrim :: PrimMonad m => ST s a -> m a
{-# INLINE unsafeSTToPrim #-}
unsafeSTToPrim = unsafePrimToPrim

-- | Convert an 'IO' action to any 'PrimMonad'. This operation is highly
-- unsafe!
unsafeIOToPrim :: PrimMonad m => IO a -> m a
{-# INLINE unsafeIOToPrim #-}
unsafeIOToPrim = unsafePrimToPrim

unsafeInlinePrim :: PrimBase m => m a -> a
{-# INLINE unsafeInlinePrim #-}
unsafeInlinePrim m = unsafeInlineIO (unsafePrimToIO m)

unsafeInlineIO :: IO a -> a
{-# INLINE unsafeInlineIO #-}
unsafeInlineIO m = case internal m realWorld# of (# _, r #) -> r

unsafeInlineST :: ST s a -> a
{-# INLINE unsafeInlineST #-}
unsafeInlineST = unsafeInlinePrim

touch :: PrimMonad m => a -> m ()
{-# INLINE touch #-}
touch x = unsafePrimToPrim
        $ (primitive (\s -> case touch# x s of { s' -> (# s', () #) }) :: IO ())

-- | Create an action to force a value; generalizes 'Control.Exception.evaluate'
evalPrim :: forall a m . PrimMonad m => a -> m a
#if MIN_VERSION_base(4,4,0)
evalPrim a = primitive (\s -> seq# a s)
#else
-- This may or may not work so well, but there's probably nothing better to do.
{-# NOINLINE evalPrim #-}
evalPrim a = unsafePrimToPrim (evaluate a :: IO a)
#endif
