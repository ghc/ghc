{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonoLocalBinds #-}

module Test where

import Data.Kind
import GHC.Exts

--import Control.Monad.Primitive -- primitive-0.7.4.0
--import Data.Primitive.MutVar -- primitive-0.7.4.0

class Monad m => PrimMonad m where
  type PrimState m
  primitive :: (State# (PrimState m) -> (# State# (PrimState m), a #)) -> m a

data MutVar s a = MutVar (MutVar# s a)

newMutVar :: PrimMonad m => a -> m (MutVar (PrimState m) a)
newMutVar = error "urk"

writeMutVar :: PrimMonad m => MutVar (PrimState m) a -> a -> m ()
writeMutVar  = error "Urk"

-----------

class Monad m => New a m where
  new :: m a

class Monad m => Add a m e | a -> e where
  add :: a -> e -> m ()

data T (m :: Type -> Type) = T

instance PrimMonad m => New (T m) m where
  new = return T

instance PrimMonad m => Add (T m) m Int where
  add _ _ = return ()

test1 :: forall m. PrimMonad m => m ()
test1 = do
  ref <- newMutVar (undefined :: T m)
  let g () = do
        t <- new
        add t (0 :: Int)
        writeMutVar ref t
  g ()

test2 :: forall m. PrimMonad m => m ()
test2 = do
  (ref :: MutVar (PrimState m) (T m)) <- newMutVar undefined
  let g () = do
        t <- new
        add t (0 :: Int)
        writeMutVar ref t
  g ()
