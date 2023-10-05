{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module T23938A where

import GHC.Exts
import GHC.ST
import Data.Kind

class Monad m => PrimMonad m where
  type PrimState m
  primitive :: (State# (PrimState m) -> (# State# (PrimState m), a #)) -> m a

instance PrimMonad (ST s) where
  type PrimState (ST s) = s
  primitive = ST
  {-# INLINE primitive #-}

{-# INLINE stToPrim #-}
stToPrim (ST m) = primitive m

data family MVector s a
data instance MVector s Int = MyVector (MutableByteArray# s)

data T (x :: Type)

data family GVector s a
data instance GVector s (T a) = MV_2 (MVector s a)

new :: (PrimMonad m) => CVector a -> () -> m (GVector (PrimState m) (T a))
{-# INLINE new #-}
new e _ = stToPrim (unsafeNew e >>= \v -> ini e v >> return v)

ini :: CVector a -> GVector s (T a) -> ST s ()
ini e (MV_2 as) = basicInitialize e as

unsafeNew :: (PrimMonad m) => CVector a -> m (GVector (PrimState m) (T a))
{-# INLINE unsafeNew #-}
unsafeNew e = stToPrim (basicUnsafeNew e >>= \(!z) -> pure (MV_2 z))

data CVector a = CVector {
  basicUnsafeNew  :: forall s. ST s (MVector s a),
  basicInitialize :: forall s. MVector s a -> ST s ()
}

f :: CVector Int
f = CVector {
  basicUnsafeNew = ST (\s -> case newByteArray# 4# s of
                              (# s', a #) -> (# s', MyVector a #)),

  basicInitialize = \(MyVector dst) ->
    ST (\s -> case setByteArray# dst 0# 0# 0# s of s' -> (# s', () #))
}
{-# INLINE f #-}

