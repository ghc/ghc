-- |
-- Module      : Basement.Monad
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- Allow to run operation in ST and IO, without having to
-- distinguinsh between the two. Most operations exposes
-- the bare nuts and bolts of how IO and ST actually
-- works, and relatively easy to shoot yourself in the foot
--
-- this is highly similar to the Control.Monad.Primitive
-- in the primitive package
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
module Basement.Monad
    ( PrimMonad(..)
    , MonadFailure(..)
    , unPrimMonad_
    , unsafePrimCast
    , unsafePrimToST
    , unsafePrimToIO
    , unsafePrimFromIO
    , primTouch
    ) where

import qualified Prelude
import           GHC.ST
import           GHC.STRef
import           GHC.IORef
import           GHC.IO
import           GHC.Prim
import           Basement.Compat.Base (Exception, (.), ($), Applicative, Monad)
import           Basement.Compat.Primitive

-- | Primitive monad that can handle mutation.
--
-- For example: IO and ST.
class (Prelude.Functor m, Applicative m, Prelude.Monad m) => PrimMonad m where
    -- | type of state token associated with the PrimMonad m
    type PrimState m
    -- | type of variable associated with the PrimMonad m
    type PrimVar m :: * -> *
    -- | Unwrap the State# token to pass to a function a primitive function that returns an unboxed state and a value.
    primitive :: (State# (PrimState m) -> (# State# (PrimState m), a #)) -> m a
    -- | Throw Exception in the primitive monad
    primThrow :: Exception e => e -> m a
    -- | Run a Prim monad from a dedicated state#
    unPrimMonad  :: m a -> State# (PrimState m) -> (# State# (PrimState m), a #)

    -- | Build a new variable in the Prim Monad
    primVarNew :: a -> m (PrimVar m a)
    -- | Read the variable in the Prim Monad
    primVarRead :: PrimVar m a -> m a
    -- | Write the variable in the Prim Monad
    primVarWrite :: PrimVar m a -> a -> m ()

-- | just like `unwrapPrimMonad` but throw away the result and return just the new State#
unPrimMonad_ :: PrimMonad m => m () -> State# (PrimState m) -> State# (PrimState m)
unPrimMonad_ p st =
    case unPrimMonad p st of
        (# st', () #) -> st'
{-# INLINE unPrimMonad_ #-}

instance PrimMonad IO where
    type PrimState IO = RealWorld
    type PrimVar IO = IORef
    primitive = IO
    {-# INLINE primitive #-}
    primThrow = throwIO
    unPrimMonad (IO p) = p
    {-# INLINE unPrimMonad #-}
    primVarNew = newIORef
    primVarRead = readIORef
    primVarWrite = writeIORef

instance PrimMonad (ST s) where
    type PrimState (ST s) = s
    type PrimVar (ST s) = STRef s
    primitive = ST
    {-# INLINE primitive #-}
    primThrow = unsafeIOToST . throwIO
    unPrimMonad (ST p) = p
    {-# INLINE unPrimMonad #-}
    primVarNew = newSTRef
    primVarRead = readSTRef
    primVarWrite = writeSTRef

-- | Convert a prim monad to another prim monad.
--
-- The net effect is that it coerce the state repr to another,
-- so the runtime representation should be the same, otherwise
-- hilary ensues.
unsafePrimCast :: (PrimMonad m1, PrimMonad m2) => m1 a -> m2 a
unsafePrimCast m = primitive (unsafeCoerce# (unPrimMonad m))
{-# INLINE unsafePrimCast #-}

-- | Convert any prim monad to an ST monad
unsafePrimToST :: PrimMonad prim => prim a -> ST s a
unsafePrimToST = unsafePrimCast
{-# INLINE unsafePrimToST #-}

-- | Convert any prim monad to an IO monad
unsafePrimToIO :: PrimMonad prim => prim a -> IO a
unsafePrimToIO = unsafePrimCast
{-# INLINE unsafePrimToIO #-}

-- | Convert any IO monad to a prim monad
unsafePrimFromIO :: PrimMonad prim => IO a -> prim a
unsafePrimFromIO = unsafePrimCast
{-# INLINE unsafePrimFromIO #-}

-- | Touch primitive lifted to any prim monad
primTouch :: PrimMonad m => a -> m ()
primTouch x = unsafePrimFromIO $ primitive $ \s -> case touch# x s of { s2 -> (# s2, () #) }
{-# INLINE primTouch #-}

-- | Monad that can represent failure
--
-- Similar to MonadFail but with a parametrized Failure linked to the Monad
class Monad m => MonadFailure m where
    -- | The associated type with the MonadFailure, representing what
    -- failure can be encoded in this monad
    type Failure m

    -- | Raise a Failure through a monad.
    mFail :: Failure m -> m ()

instance MonadFailure Prelude.Maybe where
    type Failure Prelude.Maybe = ()
    mFail _ = Prelude.Nothing
instance MonadFailure (Prelude.Either a) where
    type Failure (Prelude.Either a) = a
    mFail a = Prelude.Left a
