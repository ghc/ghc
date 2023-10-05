{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleContexts,
   TypeFamilies, ScopedTypeVariables #-}

module T7280 where

import Data.Kind (Type)

type family Mutable (v :: Type -> Type) :: Type -> Type -> Type
class MVector (v :: Type -> Type -> Type) a
class MVector (Mutable v) a => Vector v a where
   copy :: Monad m => Mutable v s a -> v a -> m ()

data Chunk v s a = Chunk (forall m. (Monad m, Vector v a) => Mutable v s a -> m ())

vstep (v:vs) = Chunk (\mv -> copy mv v)
