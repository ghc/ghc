{-# LANGUAGE MagicHash, UnboxedTuples, DeriveDataTypeable #-}

-- |
-- Module      : Data.Primitive.MutVar
-- Copyright   : (c) Justin Bonnar 2011, Roman Leshchinskiy 2011-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Primitive boxed mutable variables
--

module Data.Primitive.MutVar (
  MutVar(..),

  newMutVar,
  readMutVar,
  writeMutVar,

  atomicModifyMutVar,
  atomicModifyMutVar',
  modifyMutVar,
  modifyMutVar'
) where

import Control.Monad.Primitive ( PrimMonad(..), primitive_ )
import GHC.Prim ( MutVar#, sameMutVar#, newMutVar#,
                  readMutVar#, writeMutVar#, atomicModifyMutVar# )
import Data.Primitive.Internal.Compat ( isTrue# )
import Data.Typeable ( Typeable )

-- | A 'MutVar' behaves like a single-element mutable array associated
-- with a primitive state token.
data MutVar s a = MutVar (MutVar# s a)
  deriving ( Typeable )

instance Eq (MutVar s a) where
  MutVar mva# == MutVar mvb# = isTrue# (sameMutVar# mva# mvb#)

-- | Create a new 'MutVar' with the specified initial value
newMutVar :: PrimMonad m => a -> m (MutVar (PrimState m) a)
{-# INLINE newMutVar #-}
newMutVar initialValue = primitive $ \s# ->
  case newMutVar# initialValue s# of
    (# s'#, mv# #) -> (# s'#, MutVar mv# #)

-- | Read the value of a 'MutVar'
readMutVar :: PrimMonad m => MutVar (PrimState m) a -> m a
{-# INLINE readMutVar #-}
readMutVar (MutVar mv#) = primitive (readMutVar# mv#)

-- | Write a new value into a 'MutVar'
writeMutVar :: PrimMonad m => MutVar (PrimState m) a -> a -> m ()
{-# INLINE writeMutVar #-}
writeMutVar (MutVar mv#) newValue = primitive_ (writeMutVar# mv# newValue)

-- | Atomically mutate the contents of a 'MutVar'
atomicModifyMutVar :: PrimMonad m => MutVar (PrimState m) a -> (a -> (a,b)) -> m b
{-# INLINE atomicModifyMutVar #-}
atomicModifyMutVar (MutVar mv#) f = primitive $ atomicModifyMutVar# mv# f

-- | Strict version of 'atomicModifyMutVar'. This forces both the value stored
-- in the 'MutVar' as well as the value returned.
atomicModifyMutVar' :: PrimMonad m => MutVar (PrimState m) a -> (a -> (a, b)) -> m b
{-# INLINE atomicModifyMutVar' #-}
atomicModifyMutVar' mv f = do
  b <- atomicModifyMutVar mv force
  b `seq` return b
  where
    force x = let (a, b) = f x in (a, a `seq` b)

-- | Mutate the contents of a 'MutVar'
modifyMutVar :: PrimMonad m => MutVar (PrimState m) a -> (a -> a) -> m ()
{-# INLINE modifyMutVar #-}
modifyMutVar (MutVar mv#) g = primitive_ $ \s# ->
  case readMutVar# mv# s# of
    (# s'#, a #) -> writeMutVar# mv# (g a) s'#

-- | Strict version of 'modifyMutVar'
modifyMutVar' :: PrimMonad m => MutVar (PrimState m) a -> (a -> a) -> m ()
{-# INLINE modifyMutVar' #-}
modifyMutVar' (MutVar mv#) g = primitive_ $ \s# ->
  case readMutVar# mv# s# of
    (# s'#, a #) -> let a' = g a in a' `seq` writeMutVar# mv# a' s'#

