{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleContexts,
   TypeFamilies, ScopedTypeVariables #-}

module T7280 where

type family Mutable (v :: * -> *) :: * -> * -> *
class MVector (v :: * -> * -> *) a
class MVector (Mutable v) a => Vector v a where
   copy :: Monad m => Mutable v s a -> v a -> m ()

data Chunk v s a = Chunk (forall m. (Monad m, Vector v a) => Mutable v s a -> m ())

vstep (v:vs) = Chunk (\mv -> copy mv v)
